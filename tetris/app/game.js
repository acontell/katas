function Game(board, looper, scorer) {
    this.getBoard = () => board;
    this.start = () => this.init() && looper.start(this.tick);
    this.init = () => board.newActivePiece();
    this.tick = () => {
        let completedLines = board.getCompletedLines();
        this.updateBoard(completedLines);
        this.updateScore(completedLines);
        this.advance();
        return this;
    };
    this.updateBoard = completedLines => {
        board.clearLines(completedLines);
        board.removeEmptyPieces();
        board.collapsePieces(completedLines);
    };
    this.updateScore = completedLines => scorer.addPoints(completedLines, board.isEmpty());
    this.advance = () => board.canMoveDownActivePiece() ? board.moveDownActivePiece() : this.noAdvancePossible();
    this.noAdvancePossible = () => board.isFull() ? this.gameOver() : this.keepPlaying();
    this.gameOver = () => looper.stop();
    this.keepPlaying = () => {
        board.blockActivePiece();
        board.newActivePiece();
    };
    this.rotate = () => board.canRotateActivePiece() && board.rotateActivePiece();
    this.moveRight = () => board.canMoveRightActivePiece() && board.moveRightActivePiece();
    this.moveLeft = () => board.canMoveLeftActivePiece() && board.moveLeftActivePiece();
    this.moveDown = () => this.advance();
    this.isEnded = () => looper.isEnded();
    this.getScore = () => scorer.getScore();
}

module.exports = Game;