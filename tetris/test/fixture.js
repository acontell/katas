const _ = require('lodash');
const Game = require('../app/game');
const Board = require('../app/board');
const pieceFactory = require('../app/piece_factory');
const boardRules = require('../app/board_rules');
const Looper = require('../app/looper');
const Scorer = require('../app/scorer');
const Block = require('../app/block');
const numberOfRows = 24;
const numberOfColumns = 10;
const repaintInterval = 200;

function buildBoard(numberOfRows, numberOfColumns, pieceFactory, rules) {
    return new Board(numberOfRows, numberOfColumns, pieceFactory, rules);
}

function buildBoardWith(pieceFactory, rules) {
    return buildBoard(numberOfRows, numberOfColumns, pieceFactory, rules);
}

function buildGame() {
    return buildGameWith(buildBoardWith(pieceFactory, boardRules), repaintInterval);
}

function buildGameWith(board, repaintInterval) {
    return new Game(board, new Looper(setInterval, clearInterval, repaintInterval), new Scorer());
}

function mockBoardRules(source) {
    return _.assignIn({}, boardRules, source);
}

function generatePiecesFillingOneLine(id) {
    // Shape L => 5
    // Shape half H shape => 2
    return _.range(numberOfColumns / 2)
        .map(n => pieceFactory.getPiece(id, new Block(5, n * 2)));
}

function generatePiecesFillingTwoLines() {
    // Vertical Shape => 0, square => 1
    // 8 vertical shapes + 1 squares
    return _.range(numberOfColumns - 2)
        .map(n => pieceFactory.getPiece(0, new Block(5, n)))
        .concat([pieceFactory.getPiece(1, new Block(5, 8))]);
}

function generatePiecesThatFillLines() {
    // Vertical Shape => 0
    return _.range(numberOfColumns)
        .map(n => pieceFactory.getPiece(0, new Block(5, n)));
}

module.exports = {
    buildBoard: () => buildBoardWith(pieceFactory, boardRules),
    buildBoardWith: buildBoardWith,
    buildGame: buildGame,
    buildGameWith: buildGameWith,
    mockBoardRules: mockBoardRules,
    getRepaintInterval: () => repaintInterval,
    generatePiecesFillingOneLine: generatePiecesFillingOneLine,
    generatePiecesFillingTwoLines: generatePiecesFillingTwoLines,
    generatePiecesThatFillLines: generatePiecesThatFillLines
};
