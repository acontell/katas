const _ = require('lodash');
const expect = require('chai').expect;
const sinon = require('sinon');
const fixture = require('./fixture');
const pieceFactory = require('../app/piece_factory');
const numberOfColumns = 10;
const numberOfRows = 24;
const ultraQuickSpeed = 5;
let game;
let board;
let activePiece;
let mockBoard;

beforeEach('Setting up things', () => {
    game = fixture.buildGame();
    board = game.getBoard();
    activePiece = board.getActivePiece();
});

describe('As the game', () => {
    describe('In order to restrict play area', () => {
        it('should create a board with 10 columns', () => {
            expect(board.getNumberOfColumns()).to.equal(numberOfColumns);
        });
        it('should create a board with 24 rows', () => {
            expect(board.getNumberOfRows()).to.equal(numberOfRows);
        });
        it('should create an empty board', () => {
            expect(board.getNumberOfPieces()).to.equal(0);
        });
    });

    describe('In order to start the game', () => {
        it('should create a new piece', () => {
            game.init();
            expect(!!board.getActivePiece()).to.be.true;
        });
        it('should be created at the top center of the board', () => {
            game.init();
            expect(board.getActivePiece().getLowestBlock().equals(board.getTopCenterBlock())).to.be.true;
        });
        it('should create a random piece', () => {
            game.init();
            expect(getOneHundredInitialPieces().find(aDifferentPiece(board.getActivePiece()))).to.not.be.undefined;
        });
    });

    describe('In order to advance the game', () => {
        function givenBoardGameWithPiecesBlocked() {
            game = fixture.buildGameWith(fixture.buildBoardWith(pieceFactory, fixture.mockRules({canMoveDown: _.constant(false)})));
            board = game.getBoard();
            board.isBoardFull = _.constant(false);
        }

        function givenInitializedBoardGameWithPiecesBlocked() {
            givenBoardGameWithPiecesBlocked();
            game.init();
            activePiece = game.getBoard().getActivePiece();
        }

        function givenInitializedFullBoardGameWithExpectations() {
            givenBoardGameWithPiecesBlocked();
            mockBoard = sinon.mock(board).expects('addNewPiece').once();
            game.init();
            activePiece = game.getBoard().getActivePiece();
            board.isBoardFull = _.constant(true);
        }

        it('should not create a new piece if the game is over', () => {
            givenInitializedFullBoardGameWithExpectations();
            game.tick();
            expect(game.getBoard().getActivePiece()).to.be.equal(activePiece);
            mockBoard.verify();
        });
        it('should create a new piece if there is no active piece', () => {
            givenInitializedBoardGameWithPiecesBlocked();
            game.tick();
            expect(game.getBoard().getActivePiece()).to.be.not.equal(activePiece);
        });
        it('should create the new piece in the top center of the board', () => {
            givenInitializedBoardGameWithPiecesBlocked();
            game.tick();
            expect(game.getBoard().getActivePiece().getLowestBlock().equals(board.getTopCenterBlock())).to.be.true;
        });
    });

    describe('In order to end the game', () => {
        it('should end the game when the next active piece comes to rest on top of the board', () => {
            let game = fixture.buildGameWith(fixture.buildBoard(), ultraQuickSpeed);
            game.start();
            return advanceUntilGameOver(game);
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
    let nGame = fixture.buildGame();
    nGame.init();
    return nGame;
}

function advanceUntilGameOver(game) {
    // if motion loop is not broken, it will never end and Mocha will complain.
    return advance(game)
        .then(isGameOver => isGameOver || advanceUntilGameOver(game));
}

function advance(game) {
    return new Promise(resolve => _.delay(() => resolve(game.isEnded()), ultraQuickSpeed * numberOfRows));
}