const _ = require('lodash');
const Game = require('../app/game');
const Board = require('../app/board');
const pieceFactory = require('../app/piece_factory');
const rules = require('../app/rules');
const numberOfRows = 24;
const numberOfColumns = 10;

function buildBoard(numberOfRows, numberOfColumns, pieceFactory, rules) {
    return new Board(numberOfRows, numberOfColumns, pieceFactory, rules);
}

function buildBoardWith(pieceFactory, rules) {
    return buildBoard(numberOfRows, numberOfColumns, pieceFactory, rules);
}

function buildGame() {
    return buildGameWith(buildBoardWith(pieceFactory, rules));
}

function buildGameWith(board, fps) {
    return new Game(board, fps);
}

function mockRules(source) {
    return _.assignIn({}, rules, source);
}

module.exports = {
    buildBoard: () => buildBoardWith(pieceFactory, rules),
    buildBoardWith: buildBoardWith,
    buildGame: buildGame,
    buildGameWith: buildGameWith,
    mockRules: mockRules
};
