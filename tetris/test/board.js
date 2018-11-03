const _ = require('lodash');
const expect = require('chai').expect;
const sinon = require('sinon');
const fixture = require('./fixture');
const boardRules = require('../app/board_rules');
const pieceFactory = require('../app/piece_factory');
const Block = require('../app/block');
let game;
let board;
let initialBlock = new Block(0, 0);
let activePiece;
const noRotateMsg = 'no rotation';
let mockBoardRules;
let mock;

beforeEach('Setting up things', () => {
    game = fixture.buildGame();
    board = game.getBoard();
    activePiece = pieceFactory.getRandomPiece(initialBlock);
    mockBoardRules = fixture.mockBoardRules({
        canMoveDown: _.constant(false),
        canAddPiece: _.constant(true),
        canRotate: _.constant(noRotateMsg)
    });
});

describe('As the game', () => {
    describe('In order to advance the game', () => {
        function givenBoardWithMockBoardRules() {
            game = fixture.buildGameWith(fixture.buildBoardWith(pieceFactory, mockBoardRules));
            board = game.getBoard();
        }

        function givenBoardWithBlockedPiecesButNotFull() {
            givenBoardWithMockBoardRules();
            board.isBoardFull = _.constant(false);
        }

        function givenInitAndTick() {
            game.init();
            game.tick();
        }

        function rotateAndVerify() {
            game.rotateActivePiece();
            mock.verify();
        }

        it('should move the active piece one unit down', () => {
            givenInitAndTick();
            expect(board.getActivePiece().getLowestBlock().getRow()).to.equal(board.getTopCenterBlock().getRow() + 1);
        });
        it('should move it at a regular interval', () => {
            game.start();
            return advanceThreeTicks().then(assertActivePieceHasMovedAndStop);
        });
        it('should make not movable the active piece when it cannot go down anymore', () => {
            givenBoardWithBlockedPiecesButNotFull();
            game.init();
            activePiece = game.getBoard().getActivePiece();
            game.tick();
            // Only active piece is movable.
            expect(game.getBoard().getActivePiece()).to.be.not.equal(activePiece);
        });
        it('should detect active piece is stopped when another piece prevents it to go down anymore', () => {
            let pieces = [pieceFactory.getRandomPiece(new Block(initialBlock.getRow() + 1, initialBlock.getColumn()))];
            expect(boardRules.canMoveDown(activePiece, pieces, 15)).to.be.false;
        });
        it('should detect active piece is stopped when it reaches the bottom of the board', () => {
            expect(boardRules.canMoveDown(pieceFactory.getRandomPiece(new Block(15, 0)), [], 15)).to.be.false;
        });
        it('should move when no other piece prevents it to go down', () => {
            let pieces = [pieceFactory.getRandomPiece(new Block(initialBlock.getRow() + 5, initialBlock.getColumn()))];
            expect(boardRules.canMoveDown(activePiece, pieces, 15)).to.be.true;
        });
        it('should be able to advance when the bottom is far', () => {
            expect(boardRules.canMoveDown(pieceFactory.getRandomPiece(new Block(13, 0)), [], 15)).to.be.true;
        });
        it('should say that the board is full when no new piece can be added', () => {
            givenBoardWithMockBoardRules();
            mockBoardRules.canAddPiece = _.constant(false);
            expect(board.isBoardFull()).to.be.true;
        });
        it('should say that the board is not full when new pieces can be added', () => {
            givenBoardWithMockBoardRules();
            expect(board.isBoardFull()).to.be.false;
        });
        it('should update board every tick', () => {
            mock = sinon.mock(game).expects('update').once();
            givenInitAndTick();
            mock.verify();
        });
        it('should be able to clear line and leave pieces incomplete', () => {
            board.addPieces(generatePiecesFillingOneLine(5));
            game.update(board.getCompletedLines());
            expect(_.every(board.getPieces(), piece => _.size(piece.getBlocks()) === 2)).to.be.true;
        });
        it('should be able to clear lines and leave pieces incomplete', () => {
            board.addPieces(generatePiecesFillingTwoLines());
            game.update(board.getCompletedLines());
            expect(_.every(board.getPieces(), piece => _.size(piece.getBlocks()) === 2)).to.be.true;
        });
        it('should be able to clear lines and remove pieces without blocks', () => {
            board.addPieces(generatePiecesFillingTwoLines());
            game.update(board.getCompletedLines());
            expect(_.size(board.getPieces())).to.equal(8);
        });
        it('should be able to clear pieces completely', () => {
            board.addPieces(generatePiecesThatFillLines());
            game.update(board.getCompletedLines());
            expect(_.size(board.getPieces())).to.equal(0);
        });
        it('should move down blocks one unit when some row below has disappeared', () => {
            board.addPieces(generatePiecesFillingOneLine(2));
            game.update(board.getCompletedLines());
            expect(_.every(board.getPieces(), piece => piece.getHighestBlock().getRow() === 4)).to.be.true;
        });
        it('should move down blocks n units when n rows below have disappeared', () => {
            board.addPieces(generatePiecesFillingTwoLines());
            game.update(board.getCompletedLines());
            expect(_.every(board.getPieces(), piece => piece.getHighestBlock().getRow() === 4)).to.be.true;
        });
        it('should prevent rotation when there is collision with the bottom', () => {
            let result = boardRules.canRotate(pieceFactory.getPiece(0, new Block(16, 0)), [], 15, 10);
            expect(result).to.be.false;
        });
        it('should prevent rotation when there is collision with the left side', () => {
            let result = boardRules.canRotate(pieceFactory.getPiece(0, new Block(0, -1)), [], 15, 10);
            expect(result).to.be.false;
        });
        it('should prevent rotation when there is collision with the right side', () => {
            let result = boardRules.canRotate(pieceFactory.getPiece(0, new Block(0, 11)), [], 15, 10);
            expect(result).to.be.false;
        });
        it('should prevent rotation when there is collision with other piece on the bottom', () => {
            let pieces = [pieceFactory.getPiece(0, new Block(5, 11))];
            let result = boardRules.canRotate(pieceFactory.getPiece(0, new Block(5, 11)), pieces, 30, 30);
            expect(result).to.be.false;
        });
        it('should prevent rotation when there is collision with other piece on the side', () => {
            let pieces = [pieceFactory.getPiece(0, new Block(5, 10))];
            let result = boardRules.canRotate(pieceFactory.getPiece(0, new Block(3, 10), 1), pieces, 30, 30);
            expect(result).to.be.false;
        });
        it('should rotate when there are no obstacles', () => {
            let pieces = [pieceFactory.getPiece(0, new Block(5, 10))];
            let result = boardRules.canRotate(pieceFactory.getPiece(0, new Block(5, 9), 1), pieces, 30, 30);
            expect(result).to.be.true;
        });
        it('should not rotate piece when piece cannot be rotated (game)', () => {
            board.canRotatePiece = _.constant(false);
            mock = sinon.mock(board).expects('rotateActivePiece').never();
            rotateAndVerify();
        });
        it('should rotate piece when piece can be rotated (game)', () => {
            board.canRotatePiece = _.constant(true);
            mock = sinon.mock(board).expects('rotateActivePiece').once();
            rotateAndVerify();
        });
        it('should call board rules when checking if piece can be rotated', () => {
            givenBoardWithBlockedPiecesButNotFull();
            game.init();
            expect(board.canRotatePiece()).to.be.equal(noRotateMsg);
        });
        it('should change active piece when piece is rotated', () => {
            game.init();
            board.rotateActivePiece();
            expect(board.getActivePiece()).to.be.not.equal(board.rotateActivePiece());
        });
    });
});

function advanceThreeTicks() {
    return new Promise(resolve => {
        _.delay(() => {
            resolve(board.getActivePiece().getLowestBlock().getRow());
        }, game.getRepaintInterval() * 3 + game.getRepaintInterval() / 2);
    });
}

function assertActivePieceHasMovedAndStop(newActivePieceRow) {
    expect(newActivePieceRow).to.equal(board.getTopCenterBlock().getRow() + 3);
    game.gameOver();
}

function generatePiecesFillingOneLine(id) {
    // Shape L => 5
    // Shape half H shape => 2
    return _.range(board.getNumberOfColumns() / 2)
        .map(n => pieceFactory.getPiece(id, new Block(5, n * 2)));
}

function generatePiecesFillingTwoLines() {
    // Vertical Shape => 0, square => 1
    // 8 vertical shapes + 1 squares
    return _.range(board.getNumberOfColumns() - 2)
        .map(n => pieceFactory.getPiece(0, new Block(5, n)))
        .concat([pieceFactory.getPiece(1, new Block(5, 8))]);
}

function generatePiecesThatFillLines() {
    // Vertical Shape => 0
    return _.range(board.getNumberOfColumns())
        .map(n => pieceFactory.getPiece(0, new Block(5, n)));
}