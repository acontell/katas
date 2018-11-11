const _ = require('lodash');
const expect = require('chai').expect;
const sinon = require('sinon');
const buildContext = require('../ui/js/context');
const MatrixConverter = require('../ui/js/matrix_converter');
const CanvasLooper = require('../ui/js/canvas_looper');
const Pencil = require('../ui/js/pencil');
const addEvents = require('../ui/js/events');
const defaultValue = -1;
const converter = new MatrixConverter(defaultValue);
const conf = {
    canvasId: 'id',
    height: 12,
    width: 24,
    numberOfRows: 2,
    numberOfColumns: 2,
    font: 'hola',
    colors: {
        1: 'red'
    },
    gameOverMessage: 'bie',
    gameOverX: 4,
    gameOverY: 5,
    squareSide: 3,
    scoreMessage: 'score',
    scoreX: 1,
    scoreY: 2
};
let mockDocument;
let mockStuckPiece;
let mockBlock;
let mockBlock1;
let mockActivePiece;
let mockBoard;
let mockCtx;
let mockWindow;
let mockPencil;
let mockConverter;
let mockGame;

beforeEach('Set up', () => {
    mockDocument = {
        getElementById: canvasId => canvasId === conf.canvasId ? mockDocument : null,
        getContext: value => value === '2d' ? mockCtx : null,
        addEventListener: (eventName, cbk) => ev => eventName === 'keydown' ? cbk(ev) : null,
    };
    mockCtx = {
        canvas: {
            height: conf.height,
            width: conf.width
        },
        fillText: _.noop,
        clearRect: _.noop,
        fillRect: _.noop,
        beginPath: _.noop,
        moveTo: _.noop,
        lineTo: _.noop,
        stroke: _.noop
    };
    mockBlock = {getRow: _.constant(0), getColumn: _.constant(0)};
    mockBlock1 = {getRow: _.constant(1), getColumn: _.constant(1)};
    mockStuckPiece = {getBlocks: _.constant([mockBlock]), getId: _.constant(1)};
    mockActivePiece = {getBlocks: _.constant([mockBlock1]), getId: _.constant(2)};
    mockBoard = {
        getNumberOfRows: _.constant(conf.numberOfRows),
        getNumberOfColumns: _.constant(conf.numberOfColumns),
        getAllPieces: _.constant([mockStuckPiece, mockActivePiece])
    };
    mockWindow = {
        cancelAnimationFrame: _.noop,
        requestAnimationFrame: cbk => {
            cbk();
            return 'hola';
        },
        setTimeout: _.noop
    };
    mockPencil = {
        drawGame: _.noop,
        drawGameOver: _.constant('hola')
    };
    mockConverter = {convert: _.identity};
    mockGame = {
        getBoard: _.constant('board'),
        getScore: _.constant('score')
    };
});

describe('As the player', () => {
    describe('In order to play the game', () => {
        describe('I want to view the board', () => {
            it('should create a context for the canvas with predefined width and height', () => {
                let result = buildContext(mockDocument, conf);
                expect(result.canvas.width).to.be.equal(conf.width);
                expect(result.canvas.height).to.be.equal(conf.height);
            });

            it('should convert from board object to matrix representation', () => {
                let result = converter.convert(mockBoard);
                expect(result[mockBlock.getRow()][mockBlock.getColumn()]).to.be.equal(mockStuckPiece.getId());
                expect(result[mockBlock1.getRow()][mockBlock1.getColumn()]).to.be.equal(mockActivePiece.getId());
                expect(result[0][1]).to.be.equal(defaultValue);
                expect(result[1][0]).to.be.equal(defaultValue);
            });

            it('should cancel animation frame with the id of the animation frame when execution is stopped', () => {
                let mock = sinon.mock(mockWindow).expects('cancelAnimationFrame').withArgs(mockWindow.requestAnimationFrame(_.noop)).once();
                let canvasLooper = new CanvasLooper(mockWindow, mockPencil, mockConverter);
                canvasLooper.start(_.constant(mockGame), 12);
                canvasLooper.stop();
                mock.verify();
            });

            it('should requestAnimationFrame drawing "game over" when the execution is stopped', () => {
                let mock = sinon.mock(mockWindow).expects('requestAnimationFrame').withArgs(mockPencil.drawGameOver).once();
                let canvasLooper = new CanvasLooper(mockWindow, mockPencil, mockConverter);
                canvasLooper.stop();
                mock.verify();
            });

            it('should stop animation when the execution is stopped', () => {
                let mock = sinon.mock(mockWindow).expects('setTimeout').once();
                let canvasLooper = new CanvasLooper(mockWindow, mockPencil, mockConverter);
                let tickFnc = _.constant(mockGame);
                canvasLooper.start(tickFnc, 12);
                canvasLooper.stop();
                canvasLooper.start(tickFnc, 12);
                mock.verify();
            });

            it('should paint screen in each iteration', () => {
                let mock = sinon.mock(mockPencil).expects('drawGame').withArgs(mockGame.getBoard(), mockGame.getScore()).once();
                let canvasLooper = new CanvasLooper(mockWindow, mockPencil, mockConverter);
                canvasLooper.start(_.constant(mockGame), 12);
                mock.verify();
            });

            it('should loop over with a regular interval', () => {
                let mock = sinon.mock(mockPencil).expects('drawGame').withArgs(mockGame.getBoard(), mockGame.getScore()).thrice();
                mockWindow.setTimeout = setTimeout;
                let canvasLooper = new CanvasLooper(mockWindow, mockPencil, mockConverter);
                canvasLooper.start(_.constant(mockGame), 100);
                return new Promise(_.partial(waitAndResolve, canvasLooper)).then(() => mock.verify());
            });

            function waitAndResolve(canvasLooper, resolve) {
                setTimeout(() => {
                    canvasLooper.stop();
                    resolve(true);
                }, 250);
            }

            it('should paint "game over" with font from conf', () => {
                new Pencil(mockCtx, conf).drawGameOver();
                expect(mockCtx.font).to.be.equal(conf.font);
            });

            it('should paint "game over" with font from conf', () => {
                new Pencil(mockCtx, conf).drawGameOver();
                expect(mockCtx.font).to.be.equal(conf.font);
            });

            it('should paint "game over" with fillText in coordinates from conf', () => {
                let mock = sinon.mock(mockCtx).expects('fillText').withArgs(conf.gameOverMessage, conf.gameOverX, conf.gameOverY).once();
                new Pencil(mockCtx, conf).drawGameOver();
                mock.verify();
            });

            it('should clear canvas when painting game', () => {
                let mock = sinon.mock(mockCtx).expects('clearRect').withArgs(0, 0, conf.width, conf.height).once();
                new Pencil(mockCtx, conf).drawGame([[]], 15);
                mock.verify();
            });

            it('should draw matrix when painting game', () => {
                let mock = sinon.mock(mockCtx).expects('fillRect').withArgs(0, 0, conf.squareSide, conf.squareSide).once();
                new Pencil(mockCtx, conf).drawGame([[1]], 15);
                mock.verify();
            });

            it('should draw separating line when painting game', () => {
                let mock = sinon.mock(mockCtx).expects('stroke').once();
                new Pencil(mockCtx, conf).drawGame([[1]], 15);
                mock.verify();
            });

            it('should draw score when painting game', () => {
                let mock = sinon.mock(mockCtx).expects('fillText').withArgs(conf.scoreMessage + 15, conf.scoreX, conf.scoreY).once();
                new Pencil(mockCtx, conf).drawGame([[1]], 15);
                mock.verify();
            });
        });
        describe('I want to interact with the game', () => {
            it('should add events for the game keys', () => {
                let evFnc = addEvents(mockDocument, {3: 'action'}, {'action': _.constant('executed')});
                expect(evFnc({keyCode: 3})).to.be.equal('executed');
                expect(evFnc({keyCode: 5})).to.be.undefined;
            });
        });
    });
});
