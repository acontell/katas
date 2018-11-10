const Game = require('../../app/game');
const Board = require('../../app/board');
const Looper = require('../../app/looper');
const CanvasLooper = require('./loopers');
const Pencil = require('./pencil');
const MatrixConverter = require('./converters');
const Scorer = require('../../app/scorer');
const pieceFactory = require('../../app/piece_factory');
const boardRules = require('../../app/board_rules');
const addEvents = require('./events');
const buildContext = require('./context');
const conf = require('./configuration');

const converter = new MatrixConverter(conf.emptyCellKey);
const ctx = buildContext(document, conf);
const pencil = new Pencil(ctx, conf.pencilConf);
const canvasLooper = new CanvasLooper(window, pencil, converter);
const looper = new Looper(canvasLooper.start, canvasLooper.stop, conf.fps);
const board = new Board(conf.numberOfRows, conf.numberOfColumns, pieceFactory, boardRules);
const scoreConf = conf.scoreConf;
const scorer = new Scorer(scoreConf.linePoints, scoreConf.tetris, scoreConf.backToBack);
const game = new Game(board, looper, scorer);

addEvents(document, conf.keys, game);
game.start();