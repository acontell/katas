const _ = require('lodash');
const expect = require('chai').expect;
const sinon = require('sinon');
const fixture = require('./fixture');
const math = require('../app/math');
const pieceFactory = require('../app/piece_factory');
const Block = require('../app/block');
let game;
let board;
let initialBlock = new Block(0, 0);
let activePiece;
let mockMath = fixture.mockMath({canMoveDown: _.constant(false), clearLines: _.constant([])});

beforeEach('Setting up things', () => {
    game = fixture.buildGame();
    board = game.getBoard();
    activePiece = pieceFactory.getRandomPiece(initialBlock);
});

describe('As the game', () => {
    describe('In order to advance the game', () => {
        function givenBoardWithBlockedPiecesButNotFull() {
            game = fixture.buildGameWith(fixture.buildBoardWith(pieceFactory, mockMath));
            board = game.getBoard();
            board.isBoardFull = _.constant(false);
        }

        function givenInitAndTick() {
            game.init();
            game.tick();
        }

        it('should move the active piece one unit down', () => {
            givenInitAndTick();
            expect(board.getActivePiece().getInitialBlock().getRow()).to.equal(board.getTopCenterBlock().getRow() + 1);
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
            let pieces = [
                pieceFactory.getRandomPiece(new Block(initialBlock.getRow() + 1, initialBlock.getColumn())),
                activePiece
            ];
            expect(math.canMoveDown(activePiece, pieces, 15)).to.be.false;
        });
        it('should detect active piece is stopped when it reaches the bottom of the board', () => {
            expect(math.canMoveDown(pieceFactory.getRandomPiece(new Block(15, 0)), [], 15)).to.be.false;
        });
        it('should move when no other piece prevents it to go down', () => {
            let pieces = [
                pieceFactory.getRandomPiece(new Block(initialBlock.getRow() + 5, initialBlock.getColumn())),
                activePiece
            ];
            expect(math.canMoveDown(activePiece, pieces, 15)).to.be.true;
        });
        it('should be able to advance when the bottom is far', () => {
            expect(math.canMoveDown(pieceFactory.getRandomPiece(new Block(14, 0)), [], 15)).to.be.true;
        });
        it('should clear lines every tick (game calls board)', () => {
            let mock = sinon.mock(board).expects('clearLines').once();
            givenInitAndTick();
            mock.verify();
        });
        it('should clear lines every tick (board calls helper and assigns pieces to the result of the call)', () => {
            game = fixture.buildGameWith(fixture.buildBoardWith(pieceFactory, mockMath));
            givenInitAndTick();
            expect(game.getBoard().getNumberOfPieces()).to.equal(0);
        });
        it('should be able to clear line and leave pieces incomplete', () => {
            let result = math.clearLines(generatePiecesFillingOneLine(), board.getNumberOfColumns());
            expect(_.every(result, piece => _.size(piece.getBlocks()) === 2)).to.be.true;
        });
        it('should be able to clear lines and leave pieces incomplete', () => {
            let result = math.clearLines(generatePiecesFillingTwoLines(), board.getNumberOfColumns());
            expect(_.every(result, piece => _.size(piece.getBlocks()) === 2)).to.be.true;
        });
        it('should be able to clear lines and remove pieces without blocks', () => {
            let result = math.clearLines(generatePiecesFillingTwoLines(), board.getNumberOfColumns());
            expect(_.size(result)).to.equal(8);
        });
        it('should be able to clear pieces completely', () => {
            let result = math.clearLines(generatePiecesThatFillLines(), board.getNumberOfColumns());
            expect(_.size(result)).to.equal(0);
        });
    });
});

function advanceThreeTicks() {
    return new Promise(resolve => {
        _.delay(() => {
            resolve(board.getActivePiece().getInitialBlock().getRow());
        }, game.getFps() * 3 + game.getFps() / 2);
    });
}

function assertActivePieceHasMovedAndStop(newActivePieceRow) {
    expect(newActivePieceRow).to.equal(board.getTopCenterBlock().getRow() + 3);
    game.gameOver();
}

function generatePiecesFillingOneLine() {
    // Shape L => 5
    return _.range(board.getNumberOfColumns())
        .map(n => pieceFactory.getPiece(5, new Block(5, n * 2)));
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