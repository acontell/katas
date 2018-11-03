function Game(board, gameActions, scorer) {
    this.getBoard = () => board;
    this.start = () => this.init() && gameActions.startLoop(this.tick);
    this.init = () => board.addNewPiece();
    this.tick = () => {
        let completedLines = board.getCompletedLines();
        this.update(completedLines);
        scorer.addPoints(completedLines, board.isBoardEmpty());
        this.advance();
        return this;
    };
    this.update = completedLines => {
        board.clearLines(completedLines);
        board.removeEmptyPieces();
        board.collapsePieces(completedLines);
    };
    this.advance = () => board.canMoveActivePiece() ? board.moveActivePiece() : this.noAdvancePossible();
    this.noAdvancePossible = () => board.isBoardFull() ? this.gameOver() : this.keepPlaying();
    this.gameOver = () => gameActions.stopLoop();
    this.keepPlaying = () => {
        board.blockActivePiece();
        board.addNewPiece();
    };
    this.rotateActivePiece = () => board.canRotatePiece() && board.rotateActivePiece();
    this.moveRight = () => board.canMoveRight() && board.moveRight();
    this.moveLeft = () => board.canMoveLeft() && board.moveLeft();
    this.isEnded = () => gameActions.isLoopEnded();
    this.getScore = () => scorer.getScore();
}

module.exports = Game;