const _ = require('lodash');
const Game = require('../app/game');
const Board = require('../app/board');
const pieceFactory = require('../app/piece_factory');
const boardRules = require('../app/board_rules');
const GameActions = require('../app/game_actions');
const gameActions = new GameActions(setInterval, clearInterval);
const numberOfRows = 24;
const numberOfColumns = 10;

function buildBoard(numberOfRows, numberOfColumns, pieceFactory, rules) {
    return new Board(numberOfRows, numberOfColumns, pieceFactory, rules);
}

function buildBoardWith(pieceFactory, rules) {
    return buildBoard(numberOfRows, numberOfColumns, pieceFactory, rules);
}

function buildGame() {
    return buildGameWith(buildBoardWith(pieceFactory, boardRules));
}

function buildGameWith(board, repaintInterval) {
    return new Game(board, repaintInterval, gameActions);
}

function mockBoardRules(source) {
    return _.assignIn({}, boardRules, source);
}

function mockGameRules(source) {
    return _.assignIn({}, gameActions, source);
}

module.exports = {
    buildBoard: () => buildBoardWith(pieceFactory, boardRules),
    buildBoardWith: buildBoardWith,
    buildGame: buildGame,
    buildGameWith: buildGameWith,
    mockBoardRules: mockBoardRules,
    mockGameRules: mockGameRules
};
