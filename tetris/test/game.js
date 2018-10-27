const expect = require("chai").expect;
const Game = require("../app/game");
let game;

beforeEach('Setting up things', () => game = new Game);

describe("As the game", () => {
    describe("In order to restrict play area", () => {
        it("should create a board with 10 columns", () => {
            expect(game.getBoard().getNumberOfColumns()).to.equal(10);
        });
        it("should create a board with 24 rows", () => {
            expect(game.getBoard().getNumberOfRows()).to.equal(24);
        });
        it("should create an empty board", () => {
            expect(game.getBoard().getNumberOfPieces()).to.equal(0);
        });
    });

    describe("In order to start the game", () => {
        it("should create a new piece", () => {
            game.start();
            expect(game.getBoard().getNumberOfPieces()).to.equal(1);
        });
        it("should be created at the top center of the board", () => {
            game.start();
            expect(game.getBoard().getActivePiece()).to.equal(1);
        });
        it("should create a random piece", () => {
        });
    });
});