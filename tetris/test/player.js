const _ = require('lodash');
const expect = require('chai').expect;
const sinon = require('sinon');
const fixture = require('./fixture');
const boardRules = require('../app/board_rules');
const pieceFactory = require('../app/piece_factory');
const Block = require('../app/block');
let game;
let board;
let activePiece;

beforeEach('Setting up things', () => {
    game = fixture.buildGame();
    board = game.getBoard();
    game.init();
    activePiece = board.getActivePiece();
});

describe('As the player', () => {
    function givenBoardCanMoveHorizontally(canMove) {
        board.canMoveLeft = _.constant(canMove);
        board.canMoveRight = _.constant(canMove);
    }

    describe('In order to place pieces', () => {
        it('should check if horizontal movement is allowed', () => {
            givenBoardCanMoveHorizontally(false);
            moveHorizontallyAndAssertExpectations([sinon.mock(board).expects('canMoveRight').once(), sinon.mock(board).expects('canMoveLeft').once()]);
        });
        it('should move if horizontal movement is allowed', () => {
            givenBoardCanMoveHorizontally(true);
            moveHorizontallyAndAssertExpectations([sinon.mock(board).expects('moveRight').once(), sinon.mock(board).expects('moveLeft').once()]);
        });
        it('should not move if horizontal movement is allowed', () => {
            givenBoardCanMoveHorizontally(false);
            moveHorizontallyAndAssertExpectations([sinon.mock(board).expects('moveRight').never(), sinon.mock(board).expects('moveLeft').never()]);
        });
        it('should move the active piece to the right', () => {
            givenBoardCanMoveHorizontally(true);
            let oldRight = activePiece.getFarRightBlock().getColumn();
            game.moveRight();
            expect(activePiece.getFarRightBlock().getColumn()).to.be.equal(oldRight + 1);
        });
        it('should move the active piece to the left', () => {
            givenBoardCanMoveHorizontally(true);
            let oldRight = activePiece.getFarRightBlock().getColumn();
            game.moveLeft();
            expect(activePiece.getFarRightBlock().getColumn()).to.be.equal(oldRight - 1);
        });
        it('should prevent movement when there is collision on the left side', () => {
            let result = boardRules.canMoveLeft(pieceFactory.getPiece(0, new Block(3, 0)), [], 30, 30);
            expect(result).to.be.false;
        });
        it('should prevent movement when there is a collision with another piece on the left', () => {
            let pieces = [pieceFactory.getPiece(0, new Block(5, 10))];
            let result = boardRules.canMoveLeft(pieceFactory.getPiece(0, new Block(3, 11)), pieces, 30, 30);
            expect(result).to.be.false;
        });
        it('should prevent movement when there is collision on the right side', () => {
            let result = boardRules.canMoveRight(pieceFactory.getPiece(0, new Block(3, 30)), [], 30, 30);
            expect(result).to.be.false;
        });
        it('should prevent movement when there is a collision with another piece on the right', () => {
            let pieces = [pieceFactory.getPiece(0, new Block(5, 10))];
            let result = boardRules.canMoveRight(pieceFactory.getPiece(0, new Block(3, 9)), pieces, 30, 30);
            expect(result).to.be.false;
        });
        it('should let move left when there are no obstacles', () => {
            let pieces = [pieceFactory.getPiece(0, new Block(5, 10))];
            let result = boardRules.canMoveLeft(pieceFactory.getPiece(0, new Block(3, 7)), pieces, 30, 30);
            expect(result).to.be.true;
        });
        it('should let move right when there are no obstacles', () => {
            let pieces = [pieceFactory.getPiece(0, new Block(5, 10))];
            let result = boardRules.canMoveRight(pieceFactory.getPiece(0, new Block(3, 5)), pieces, 30, 30);
            expect(result).to.be.true;
        });
        it('should be able to move down', () => {
            let oldActivePieceRow = activePiece.getLowestBlock().getRow();
            game.advance();
            expect(board.getActivePiece().getLowestBlock().getRow()).to.be.equal(oldActivePieceRow + 1);
        });
    });
    describe('In order to be better than my friends', () => {
        function givenGameWithPieces(pieces) {
            board.addPieces(pieces);
            game.tick();
        }

        it('should score 100 points per one line', () => {
            givenGameWithPieces(fixture.generatePiecesFillingOneLine(5));
            expect(game.getScore()).to.be.equal(100);
        });
        it('should score n * 100 points per n lines', () => {
            givenGameWithPieces(fixture.generatePiecesFillingTwoLines());
            expect(game.getScore()).to.be.equal(200);
        });
        it('should score 800 points per Tetris', () => {
            givenGameWithPieces(fixture.generatePiecesThatFillLines().concat(pieceFactory.getPiece(1, new Block(7, 8))));
            expect(game.getScore()).to.be.equal(800);
        });
        it('should score 1200 points per back-to-back Tetris', () => {
            givenGameWithPieces(fixture.generatePiecesThatFillLines());
            expect(game.getScore()).to.be.equal(1200);

        });
        it('should accumulate score between ticks', () => {
            givenGameWithPieces(fixture.generatePiecesThatFillLines());
            board.addPieces(fixture.generatePiecesThatFillLines());
            game.tick();
            expect(game.getScore()).to.be.equal(2400);
        });
    });
});

function moveHorizontallyAndAssertExpectations(mocks) {
    game.moveRight();
    game.moveLeft();
    mocks.forEach(mock => mock.verify());
}