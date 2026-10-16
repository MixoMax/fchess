"use strict";

/* ------------------------------------------------------------------
 * Constants
 * ------------------------------------------------------------------ */

const START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
const FILES = "abcdefgh";
const GLYPH = { k: "♚", q: "♛", r: "♜", b: "♝", n: "♞", p: "♟" };
const PIECE_NAME = { q: "queen", r: "rook", b: "bishop", n: "knight" };
const PROMOTION_ORDER = "qrbn";

// Mirrors the engine's mate encoding (chess_search.f90 / server.py)
const MATE_THRESHOLD = 28000;
const MATE_SCORE = 29000;

const KNIGHT_JUMPS = [[1, 2], [2, 1], [2, -1], [1, -2], [-1, -2], [-2, -1], [-2, 1], [-1, 2]];
const KING_STEPS = [[1, 0], [-1, 0], [0, 1], [0, -1], [1, 1], [1, -1], [-1, 1], [-1, -1]];
const ROOK_DIRS = [[1, 0], [-1, 0], [0, 1], [0, -1]];
const BISHOP_DIRS = [[1, 1], [1, -1], [-1, 1], [-1, -1]];

// Castling rights lost when a piece leaves or lands on these squares
const RIGHTS_LOST_ON = { e1: "KQ", a1: "Q", h1: "K", e8: "kq", a8: "q", h8: "k" };

/* ------------------------------------------------------------------
 * DOM references
 * ------------------------------------------------------------------ */

const $ = (id) => document.getElementById(id);

const els = {
    board: $("board"),
    matchup: $("matchup"),
    status: $("status"),
    engineInfo: $("engine-info"),
    evalText: $("eval-text"),
    evalBar: $("eval-bar"),
    evalFill: $("eval-fill"),
    moveList: $("move-list"),
    error: $("error"),
    depth: $("depth"),
    undo: $("btn-undo"),
    flip: $("btn-flip"),
    newGame: $("btn-new"),
    startModal: $("start-modal"),
    promotion: $("promotion"),
    promoOptions: $("promotion-options"),
};

/* ------------------------------------------------------------------
 * Application state
 * ------------------------------------------------------------------ */

const state = {
    started: false,
    fen: START_FEN,
    playerColor: "w",     // "w" or "b"
    flipped: false,       // manual board flip on top of the default orientation
    history: [],          // snapshots of positions before each move
    moves: [],            // UCI moves played in the current game
    lastMove: null,
    allLegal: [],         // legal moves (UCI) in the current position
    legal: [],            // legal moves for the selected piece
    selected: null,       // square index of the selected piece
    evalCp: 0,            // evaluation in centipawns, White's perspective
    check: false,
    result: null,         // game-over message, or null
    busy: false,          // waiting for the engine or the server
    engineInfo: "",
    gen: 0,               // bumped on new game / undo so stale responses are ignored
};

/* ------------------------------------------------------------------
 * Square and FEN helpers
 * Board index: rank * 8 + file, with a1 = 0 and h8 = 63.
 * ------------------------------------------------------------------ */

function sqIndex(name) {
    return (name.charCodeAt(1) - 49) * 8 + (name.charCodeAt(0) - 97);
}

function sqName(idx) {
    return FILES[idx % 8] + (Math.floor(idx / 8) + 1);
}

function parseFen(fen) {
    const [placement, stm, castling, ep, half, full] = fen.split(" ");
    const board = new Array(64).fill(null);
    placement.split("/").forEach((row, i) => {
        const rank = 7 - i;
        let file = 0;
        for (const ch of row) {
            if (/\d/.test(ch)) {
                file += Number(ch);
            } else {
                board[rank * 8 + file] = ch;
                file++;
            }
        }
    });
    return {
        board,
        stm,
        castling,
        ep,
        half: Number(half),
        full: Number(full),
    };
}

function toFen(s) {
    const rows = [];
    for (let rank = 7; rank >= 0; rank--) {
        let row = "";
        let empty = 0;
        for (let file = 0; file < 8; file++) {
            const piece = s.board[rank * 8 + file];
            if (piece) {
                if (empty) row += empty;
                empty = 0;
                row += piece;
            } else {
                empty++;
            }
        }
        if (empty) row += empty;
        rows.push(row);
    }
    return [rows.join("/"), s.stm, s.castling, s.ep, s.half, s.full].join(" ");
}

function sideOf(fen) {
    return fen.split(" ")[1];
}

/* ------------------------------------------------------------------
 * Local move application (keeps the FEN in sync with the engine)
 * ------------------------------------------------------------------ */

function applyMove(s, uci) {
    const board = s.board.slice();
    const from = sqIndex(uci.slice(0, 2));
    const to = sqIndex(uci.slice(2, 4));
    const promo = uci.length === 5 ? uci[4] : null;

    const piece = board[from];
    const white = piece === piece.toUpperCase();
    const type = piece.toLowerCase();
    const captured = board[to];
    const isEnPassant = type === "p" && !captured && (from % 8) !== (to % 8);
    let ep = "-";

    if (isEnPassant) {
        board[white ? to - 8 : to + 8] = null;
    }

    if (type === "k" && Math.abs(to - from) === 2) {
        // Castling: move the rook alongside the king
        const rookFrom = to > from ? from + 3 : from - 4;
        const rookTo = (from + to) / 2;
        board[rookTo] = board[rookFrom];
        board[rookFrom] = null;
    }

    board[from] = null;
    board[to] = promo ? (white ? promo.toUpperCase() : promo) : piece;

    if (type === "p" && Math.abs(to - from) === 16) {
        ep = sqName((from + to) / 2);
    }

    const removed =
        (RIGHTS_LOST_ON[sqName(from)] || "") + (RIGHTS_LOST_ON[sqName(to)] || "");
    const castlingBase = s.castling === "-" ? "" : s.castling;
    const castling =
        castlingBase.split("").filter((c) => !removed.includes(c)).join("") || "-";

    const resetClock = type === "p" || captured || isEnPassant;

    return {
        board,
        stm: white ? "b" : "w",
        castling,
        ep,
        half: resetClock ? 0 : s.half + 1,
        full: white ? s.full : s.full + 1,
    };
}

/* ------------------------------------------------------------------
 * Check detection (the engine does not expose it)
 * ------------------------------------------------------------------ */

function isAttacked(board, sq, byWhite) {
    const f = sq % 8;
    const r = Math.floor(sq / 8);
    const want = (letter) => (byWhite ? letter.toUpperCase() : letter.toLowerCase());
    const at = (df, dr) => {
        const ff = f + df;
        const rr = r + dr;
        return ff < 0 || ff > 7 || rr < 0 || rr > 7 ? null : board[rr * 8 + ff];
    };

    // Pawns attack diagonally forward, so look one rank "behind" the target
    const pawnDir = byWhite ? -1 : 1;
    if (at(-1, pawnDir) === want("p") || at(1, pawnDir) === want("p")) return true;

    for (const [df, dr] of KNIGHT_JUMPS) {
        if (at(df, dr) === want("n")) return true;
    }
    for (const [df, dr] of KING_STEPS) {
        if (at(df, dr) === want("k")) return true;
    }

    const sliderSets = [
        [ROOK_DIRS, [want("r"), want("q")]],
        [BISHOP_DIRS, [want("b"), want("q")]],
    ];
    for (const [dirs, attackers] of sliderSets) {
        for (const [df, dr] of dirs) {
            for (let k = 1; ; k++) {
                const ff = f + df * k;
                const rr = r + dr * k;
                if (ff < 0 || ff > 7 || rr < 0 || rr > 7) break;
                const p = board[rr * 8 + ff];
                if (p) {
                    if (attackers.includes(p)) return true;
                    break;
                }
            }
        }
    }
    return false;
}

function isInCheck(s) {
    const white = s.stm === "w";
    const kingIdx = s.board.indexOf(white ? "K" : "k");
    if (kingIdx < 0) return false;
    return isAttacked(s.board, kingIdx, !white);
}

function computeResult(s, legalCount, inCheck) {
    if (legalCount === 0) {
        if (inCheck) {
            return `Checkmate. ${s.stm === "w" ? "Black" : "White"} wins.`;
        }
        return "Stalemate. Draw.";
    }
    if (s.half >= 100) {
        return "Draw by the 50-move rule.";
    }
    return null;
}

/* ------------------------------------------------------------------
 * Evaluation formatting
 * ------------------------------------------------------------------ */

function isMateScore(cp) {
    return Math.abs(cp) > MATE_THRESHOLD;
}

function formatEval(cp) {
    if (isMateScore(cp)) {
        const plies = MATE_SCORE - Math.abs(cp);
        const moves = Math.ceil(plies / 2);
        return `${cp > 0 ? "" : "−"}M${moves}`;
    }
    const pawns = cp / 100;
    return `${pawns >= 0 ? "+" : "−"}${Math.abs(pawns).toFixed(2)}`;
}

function whiteShare(cp) {
    if (isMateScore(cp)) return cp > 0 ? 100 : 0;
    return 100 / (1 + Math.exp(-cp / 400));
}

/* ------------------------------------------------------------------
 * Helpers for the current game
 * ------------------------------------------------------------------ */

function viewBlack() {
    return (state.playerColor === "b") !== state.flipped;
}

function currentTurn() {
    return sideOf(state.fen);
}

function canInteract() {
    return (
        state.started &&
        !state.busy &&
        !state.result &&
        currentTurn() === state.playerColor
    );
}

function isOwnPiece(piece) {
    return (piece === piece.toUpperCase()) === (state.playerColor === "w");
}

function findUndoIndex() {
    for (let i = state.history.length - 1; i >= 0; i--) {
        if (sideOf(state.history[i].fen) === state.playerColor) return i;
    }
    return -1;
}

function clearSelection() {
    state.selected = null;
    state.legal = [];
}

function showError(message) {
    els.error.textContent = message;
    els.error.classList.remove("hidden");
}

function hideError() {
    els.error.textContent = "";
    els.error.classList.add("hidden");
}

/* ------------------------------------------------------------------
 * Engine API
 * ------------------------------------------------------------------ */

async function api(path, params) {
    const query = new URLSearchParams(params).toString();
    let res;
    try {
        res = await fetch(`/api/v1/${path}?${query}`);
    } catch (err) {
        throw new Error("Cannot reach the engine server. Is it running?");
    }

    let data = {};
    try {
        data = await res.json();
    } catch (err) {
        // Non-JSON body; fall through with empty data
    }

    if (!res.ok) {
        const detail = data.detail;
        throw new Error(typeof detail === "string" ? detail : `Request failed (${res.status})`);
    }
    return data;
}

/* ------------------------------------------------------------------
 * Game flow
 * ------------------------------------------------------------------ */

function pushHistory() {
    state.history.push({
        fen: state.fen,
        moves: state.moves.slice(),
        lastMove: state.lastMove,
    });
}

function applyLocal(uci) {
    state.fen = toFen(applyMove(parseFen(state.fen), uci));
    state.moves.push(uci);
    state.lastMove = uci;
    clearSelection();
    state.allLegal = [];
    state.check = false;
    state.result = null;
}

async function refreshPosition(gen) {
    const fen = state.fen;
    const [legalRes, evalRes] = await Promise.all([
        api("legal_moves", { fen }),
        api("evaluate", { fen }),
    ]);
    if (gen !== state.gen) return;

    const s = parseFen(fen);
    const legalCount = legalRes.legal_moves.length;
    state.allLegal = legalRes.legal_moves;
    state.evalCp = evalRes.evaluation_cp;
    state.check = isInCheck(s);
    state.result = computeResult(s, legalCount, state.check);
    render();
}

async function engineMove(gen) {
    const data = await api("best_move", {
        fen: state.fen,
        depth: els.depth.value,
    });
    if (gen !== state.gen) return;
    if (!data.best_move) {
        throw new Error("The engine did not return a move.");
    }

    pushHistory();
    applyLocal(data.best_move);

    let nps = data.nodes / Math.max(data.time, 0.001);
    let nps_fmt = nps >= 1e6 ? `${(nps / 1e6).toFixed(2)}M` : nps >= 1e3 ? `${(nps / 1e3).toFixed(2)}k` : `${Math.round(nps)}`;
    let info = `Engine: depth ${data.depth}, ${data.nodes.toLocaleString()} nodes in ${data.time.toFixed(2)}s (${nps_fmt}/s)`;
    if (data.mate_in !== null && data.mate_in !== undefined) {
        info += data.mate_in > 0
            ? `, mate in ${data.mate_in}`
            : `, mated in ${-data.mate_in}`;
    }
    state.engineInfo = info;

    await refreshPosition(gen);
}

/* Refresh the position, then let the engine move if it is its turn. */
async function advance(gen) {
    state.busy = true;
    render();
    try {
        await refreshPosition(gen);
        if (gen !== state.gen) return;
        if (!state.result && currentTurn() !== state.playerColor) {
            await engineMove(gen);
        }
    } catch (err) {
        if (gen === state.gen) showError(err.message);
    } finally {
        if (gen === state.gen) {
            state.busy = false;
            render();
        }
    }
}

async function playerMove(uci) {
    hideError();
    pushHistory();
    applyLocal(uci);
    await advance(state.gen);
}

function undo() {
    if (state.busy) return;
    const idx = findUndoIndex();
    if (idx < 0) return;

    const snap = state.history[idx];
    state.history.length = idx;
    state.fen = snap.fen;
    state.moves = snap.moves;
    state.lastMove = snap.lastMove;
    clearSelection();
    state.allLegal = [];
    state.check = false;
    state.result = null;
    state.engineInfo = "";
    hideError();

    advance(++state.gen);
}

function startGame(choice) {
    let color = choice;
    if (choice === "random") {
        color = Math.random() < 0.5 ? "w" : "b";
    }

    state.started = true;
    state.playerColor = color;
    state.flipped = false;
    state.fen = START_FEN;
    state.history = [];
    state.moves = [];
    state.lastMove = null;
    clearSelection();
    state.allLegal = [];
    state.check = false;
    state.result = null;
    state.engineInfo = "";
    state.busy = false;

    els.startModal.classList.add("hidden");
    hideError();
    advance(++state.gen);
}

function openStartDialog() {
    els.startModal.classList.remove("hidden");
}

/* ------------------------------------------------------------------
 * Interaction
 * ------------------------------------------------------------------ */

function selectSquare(idx) {
    state.selected = idx;
    const name = sqName(idx);
    state.legal = state.allLegal.filter((m) => m.slice(0, 2) === name);
    render();
}

function onSquareClick(idx) {
    if (!canInteract()) return;

    const s = parseFen(state.fen);
    const name = sqName(idx);

    // Second click on a legal destination: make the move
    if (state.selected !== null) {
        const candidates = state.legal.filter((m) => m.slice(2, 4) === name);
        if (candidates.length) {
            clearSelection();
            if (candidates.length > 1) {
                showPromotionChoice(candidates);
                render();
            } else {
                playerMove(candidates[0]);
            }
            return;
        }
        if (idx === state.selected) {
            clearSelection();
            render();
            return;
        }
    }

    // Otherwise select one of your own pieces
    const piece = s.board[idx];
    if (piece && isOwnPiece(piece)) {
        selectSquare(idx);
        return;
    }

    clearSelection();
    render();
}

function showPromotionChoice(candidates) {
    const white = state.playerColor === "w";
    const sorted = candidates
        .slice()
        .sort((a, b) => PROMOTION_ORDER.indexOf(a[4]) - PROMOTION_ORDER.indexOf(b[4]));

    els.promoOptions.innerHTML = "";
    for (const uci of sorted) {
        const letter = uci[4];
        const btn = document.createElement("button");
        btn.type = "button";
        btn.className = `promo-choice piece ${white ? "white" : "black"}`;
        btn.textContent = GLYPH[letter];
        btn.setAttribute("aria-label", `Promote to ${PIECE_NAME[letter]}`);
        btn.addEventListener("click", () => {
            els.promotion.classList.add("hidden");
            playerMove(uci);
        });
        els.promoOptions.appendChild(btn);
    }
    els.promotion.classList.remove("hidden");
}

function hidePromotionChoice() {
    els.promotion.classList.add("hidden");
}

/* ------------------------------------------------------------------
 * Rendering
 * ------------------------------------------------------------------ */

function render() {
    renderBoard();
    renderStatus();
    renderEvaluation();
    renderMoveList();
    renderControls();
}

function renderBoard() {
    const s = parseFen(state.fen);
    const flip = viewBlack();
    const targetNames = new Set(state.legal.map((m) => m.slice(2, 4)));
    const fragment = document.createDocumentFragment();

    for (let d = 0; d < 64; d++) {
        const row = Math.floor(d / 8);
        const col = d % 8;
        const rank = flip ? row : 7 - row;
        const file = flip ? 7 - col : col;
        const idx = rank * 8 + file;
        const name = sqName(idx);
        const piece = s.board[idx];

        const sq = document.createElement("div");
        sq.className = `sq ${(rank + file) % 2 === 0 ? "dark" : "light"}`;
        sq.dataset.square = name;
        sq.setAttribute("role", "gridcell");
        sq.setAttribute("aria-label", piece ? `${name}: ${piece}` : name);

        if (state.lastMove && (state.lastMove.slice(0, 2) === name || state.lastMove.slice(2, 4) === name)) {
            sq.classList.add("last-move");
        }
        if (idx === state.selected) {
            sq.classList.add("selected");
        }
        if (state.check && piece === (s.stm === "w" ? "K" : "k")) {
            sq.classList.add("check");
        }
        if (targetNames.has(name)) {
            sq.classList.add(piece ? "capture" : "target");
        }

        if (piece) {
            const span = document.createElement("span");
            span.className = `piece ${piece === piece.toUpperCase() ? "white" : "black"}`;
            span.textContent = GLYPH[piece.toLowerCase()];
            sq.appendChild(span);
        }

        if (col === 0) {
            const label = document.createElement("span");
            label.className = "coord rank";
            label.textContent = rank + 1;
            sq.appendChild(label);
        }
        if (row === 7) {
            const label = document.createElement("span");
            label.className = "coord file";
            label.textContent = FILES[file];
            sq.appendChild(label);
        }

        sq.addEventListener("click", () => onSquareClick(idx));
        fragment.appendChild(sq);
    }

    els.board.replaceChildren(fragment);
    els.board.classList.toggle("locked", !canInteract());
}

function renderStatus() {
    const yourTurn = currentTurn() === state.playerColor;
    let text;

    if (!state.started) {
        text = "Choose a colour to begin";
    } else if (state.result) {
        text = state.result;
    } else if (!yourTurn) {
        text = "Engine is thinking…";
    } else if (state.check) {
        text = "Check! Your move";
    } else {
        text = "Your move";
    }

    els.status.textContent = text;
    els.engineInfo.textContent = state.started ? state.engineInfo : "";

    const you = state.playerColor === "w" ? "White" : "Black";
    const engine = state.playerColor === "w" ? "Black" : "White";
    els.matchup.textContent = state.started
        ? `You: ${you} · Engine: ${engine}`
        : "Play against the engine";
}

function renderEvaluation() {
    const cp = state.evalCp;
    els.evalText.textContent = formatEval(cp);
    els.evalFill.style.height = `${whiteShare(cp)}%`;
    els.evalBar.classList.toggle("flipped", viewBlack());
}

function renderMoveList() {
    const list = els.moveList;
    if (!state.moves.length) {
        list.innerHTML = '<li class="empty">No moves yet</li>';
        return;
    }

    const items = [];
    for (let i = 0; i < state.moves.length; i += 2) {
        const black = state.moves[i + 1] || "";
        items.push(
            `<li><span class="num">${i / 2 + 1}.</span><span>${state.moves[i]}</span><span>${black}</span></li>`
        );
    }
    list.innerHTML = items.join("");
    list.scrollTop = list.scrollHeight;
}

function renderControls() {
    els.undo.disabled = state.busy || findUndoIndex() < 0;
    els.flip.disabled = !state.started;
}

/* ------------------------------------------------------------------
 * Wiring
 * ------------------------------------------------------------------ */

document.querySelectorAll(".choice").forEach((btn) => {
    btn.addEventListener("click", () => startGame(btn.dataset.color));
});

els.newGame.addEventListener("click", openStartDialog);

els.undo.addEventListener("click", undo);

els.flip.addEventListener("click", () => {
    state.flipped = !state.flipped;
    render();
});

els.promotion.addEventListener("click", (event) => {
    if (event.target === els.promotion) {
        hidePromotionChoice();
        render();
    }
});

document.addEventListener("keydown", (event) => {
    if (event.key !== "Escape") return;
    if (!els.promotion.classList.contains("hidden")) {
        hidePromotionChoice();
    } else if (state.selected !== null) {
        clearSelection();
    }
    render();
});

render();
openStartDialog();