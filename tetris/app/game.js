function Game(board, interval, gameActions) {
    const repaintInterval = interval || 200;
    let isEnded;
    let loopId;

    this.getBoard = () => board;
    this.getRepaintInterval = () => repaintInterval;
    this.start = () => {
        this.init();
        loopId = gameActions.startLoop(this.tick, repaintInterval);
    };
    this.init = () => board.addNewPiece();
    this.tick = () => {
        let completedLines = board.getCompletedLines();
        board.updateBoard(completedLines);
        this.advance();
        return board.toMatrix();
    };
    this.advance = () => board.canMoveActivePiece() ? board.moveActivePiece() : this.noAdvancePossible();
    this.noAdvancePossible = () => board.isBoardFull() ? this.gameOver() : this.keepPlaying();
    this.gameOver = () => {
        gameActions.stopLoop(loopId);
        isEnded = true;
    };
    this.keepPlaying = () => {
        board.blockActivePiece();
        board.addNewPiece();
    };
    this.rotateActivePiece = () => board.canRotatePiece() && board.rotateActivePiece();
    this.moveRight = () => board.canMoveRight() && board.moveRight();
    this.moveLeft = () => board.canMoveLeft() && board.moveLeft();
    this.isEnded = () => isEnded;
}

module.exports = Game;