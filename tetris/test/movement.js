const _ = require('lodash');
const expect = require('chai').expect;
const Game = require('../app/game');
let game;
let board;

beforeEach('Setting up things', () => {
    game = new Game;
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
    expect(newActivePieceRow).to.equal(board.getTopCenterBlock().getRow() + 3);
    game.stop();
}
