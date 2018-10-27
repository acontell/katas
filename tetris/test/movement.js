const _ = require('lodash');
const expect = require('chai').expect;
const fixture = require('./fixture');
const math = require('../app/math');
const pieceFactory = require('../app/piece_factory');
const Block = require('../app/block');
let game;
let board;

beforeEach('Setting up things', () => {
    game = fixture.buildGame();
    board = game.getBoard();
});

describe('As the game', () => {
    describe('In order to advance the game', () => {
        it('should move the active piece one unit down', () => {
            game.init();
            game.tick();
            expect(board.getActivePiece().getInitialBlock().getRow()).to.equal(board.getTopCenterBlock().getRow() + 1);
        });
        it('should move it at a regular interval', () => {
            game.start();
            return advanceThreeSeconds().then(assertActivePieceHasMovedAndStop);
        });
        it('should inactive active piece when it cannot go down anymore', () => {
            let game = fixture.buildGameWith(fixture.buildBoardWith(pieceFactory, {canMoveDown: _.constant(false)}));
            game.init();
            let activePiece = game.getBoard().getActivePiece();
            game.tick();
            expect(game.getBoard().getActivePiece()).to.be.not.equal(activePiece);
        });
        it('should detect active piece is stopped when another piece prevents it to go down anymore', () => {
            // TODO
            //expect(math.canMoveDown(pieceFactory.getRandomPiece(new Block(0, 0)), [pieceFactory.getRandomPiece(new Block(1, 0))], 30)).to.be.false;
        });
        it('should detect active piece is stopped when it reaches the bottom of the board', () => {
            // TODO
            //expect(math.canMoveDown(pieceFactory.getRandomPiece(new Block(29, 0)), [], 30)).to.be.false;
        });
    });
});

function advanceThreeSeconds() {
    return new Promise(resolve => {
        _.delay(() => {
            resolve(board.getActivePiece().getInitialBlock().getRow());
        }, game.getFps() * 3 + game.getFps() / 2);
    });
}

function assertActivePieceHasMovedAndStop(newActivePieceRow) {
    game.stop();
    expect(newActivePieceRow).to.equal(board.getTopCenterBlock().getRow() + 3);
}
