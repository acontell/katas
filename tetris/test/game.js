const _ = require('lodash');
const expect = require('chai').expect;
const Game = require('../app/game');
let game;

beforeEach('Setting up things', () => game = new Game);

describe('As the game', () => {
    describe('In order to restrict play area', () => {
        it('should create a board with 10 columns', () => {
            expect(game.getBoard().getNumberOfColumns()).to.equal(10);
        });
        it('should create a board with 24 rows', () => {
            expect(game.getBoard().getNumberOfRows()).to.equal(24);
        });
        it('should create an empty board', () => {
            expect(game.getBoard().getNumberOfPieces()).to.equal(0);
        });
    });

    describe('In order to start the game', () => {
        it('should create a new piece', () => {
            game.start();
            expect(game.getBoard().getNumberOfPieces()).to.equal(1);
        });
        it('should be created at the top center of the board', () => {
            let board = game.start().getBoard();
            expect(board.getActivePiece().getInitialBlock().equals(board.getTopCenterBlock())).to.be.true;
        });
        it('should create a random piece', () => {
            let piece = new Game().start().getBoard().getActivePiece();
            let differentPiece = getOneHundredInitialPieces().find(aDifferentPiece(piece));
            expect(differentPiece).to.not.be.undefined;
        });
    });
});

function getOneHundredInitialPieces() {
    return _.range(100)
        .map(_ => new Game().start())
        .map(game => game.getBoard())
        .map(board => board.getActivePiece());
}

function aDifferentPiece(piece) {
    return aPiece => aPiece.getId() !== piece.getId();
}
