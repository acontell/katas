const Game = require('../../app/game');
const Board = require('../../app/board');
const Looper = require('../../app/looper');
const CanvasLooper = require('./canvas_looper');
const Pencil = require('./pencil');
const Scorer = require('../../app/scorer');
const pieceFactory = require('../../app/piece_factory');
const boardRules = require('../../app/board_rules');
const addEvents = require('./events');
const buildContext = require('./context');
const numberOfRows = 24;
const numberOfColumns = 10;
const squareSide = 25;
const width = numberOfColumns * squareSide;
const height = numberOfRows * squareSide;
const fps = 300;
const scoreOffset = 45;
const font = '30px Arial';
const scoreStartingPoint = 10;
const pencilConf = {
    scoreOffset: scoreOffset,
    scoreStartingPoint: scoreStartingPoint,
    squareSide: squareSide,
    font: font
};
const backToBackPoints = 1200;
const tetrisPoints = 800;
const linePoints = 100;
const looper = new CanvasLooper(new Pencil(buildContext(width, height + scoreOffset), pencilConf));
const board = new Board(numberOfRows, numberOfColumns, pieceFactory, boardRules);
const game = new Game(board, new Looper(looper.start, looper.cancel, fps), new Scorer(linePoints, tetrisPoints, backToBackPoints));

addEvents(document, game);
game.start();