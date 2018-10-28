const _ = require('lodash');
const Game = require('../app/game');
const Board = require('../app/board');
const pieceFactory = require('../app/piece_factory');
const math = require('../app/math');
const numberOfRows = 24;
const numberOfColumns = 10;

function buildBoard(numberOfRows, numberOfColumns, pieceFactory, math) {
    return new Board(numberOfRows, numberOfColumns, pieceFactory, math);
}

function buildBoardWith(pieceFactory, math) {
    return buildBoard(numberOfRows, numberOfColumns, pieceFactory, math);
}

function buildGame() {
    return buildGameWith(buildBoardWith(pieceFactory, math));
}

function buildGameWith(board, fps) {
    return new Game(board, fps);
}

function mockMath(source) {
    return _.assignIn({}, math, source);
}

module.exports = {
    buildBoard: () => buildBoardWith(pieceFactory, math),
    buildBoardWith: buildBoardWith,
    buildGame: buildGame,
    buildGameWith: buildGameWith,
    mockMath: mockMath
};
