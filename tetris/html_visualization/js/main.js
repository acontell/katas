const Game = require('../../app/game');
const Board = require('../../app/board');
const GameActions = require('../../app/game_actions');
const CanvasGameActions = require('./canvas_game_actions');
const addEvents = require('./events');
const setupContextAndGet = require('./context');
const numberOfRows = 24;
const numberOfColumns = 10;
const squareSide = 25;
const width = numberOfColumns * squareSide;
const height = numberOfRows * squareSide;
const fps = 300;
const ctx = setupContextAndGet(width, height);
const canvasActions = new CanvasGameActions(ctx, height, width, squareSide);
const board = new Board(numberOfRows, numberOfColumns, require('../../app/piece_factory'), require('../../app/board_rules'));
const game = new Game(board, fps, new GameActions(canvasActions.start, canvasActions.cancel));

addEvents(document, game);
game.start();