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
    describe('In order to restrict play area', () => {
        it('should create a board with 10 columns', () => {
            expect(board.getNumberOfColumns()).to.equal(10);
        });
        it('should create a board with 24 rows', () => {
            expect(board.getNumberOfRows()).to.equal(24);
        });
        it('should create an empty board', () => {
            expect(board.getNumberOfPieces()).to.equal(0);
        });
    });

    describe('In order to start the game', () => {
        it('should create a new piece', () => {
            game.init();
            expect(board.getNumberOfPieces()).to.equal(1);
        });
        it('should be created at the top center of the board', () => {
            game.init();
            expect(board.getActivePiece().getInitialBlock().equals(board.getTopCenterBlock())).to.be.true;
        });
        it('should create a random piece', () => {
            game.init();
            expect(getOneHundredInitialPieces().find(aDifferentPiece(board.getActivePiece()))).to.not.be.undefined;
        });
    });
});

function getOneHundredInitialPieces() {
    return _.range(100)
        .map(getInitializedGame)
        .map(game => game.getBoard())
        .map(board => board.getActivePiece());
}

function aDifferentPiece(piece) {
    return aPiece => aPiece.getId() !== piece.getId();
}

function getInitializedGame() {
    let nGame = new Game();
    nGame.init();
    return nGame;
}
