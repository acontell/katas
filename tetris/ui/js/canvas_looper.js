function CanvasLooper(window, pencil, converter) {
    const convert = converter.convert;
    const cancelAnimationFrame = window.cancelAnimationFrame;
    const requestAnimationFrame = window.requestAnimationFrame;
    const setTimeout = window.setTimeout;
    let animationId;
    let keepGoing = true;

    this.stop = () => {
        keepGoing = false;
        cancelAnimationFrame(animationId);
        requestAnimationFrame(pencil.drawGameOver);
    };

    this.start = (tickFnc, interval) => {
        function paint(game) {
            pencil.drawGame(convert(game.getBoard()), game.getScore());
        }

        function waitAndLoop() {
            setTimeout(() => animationId = requestAnimationFrame(mainLoop), interval);
        }

        function mainLoop() {
            if (keepGoing) {
                paint(tickFnc());
                waitAndLoop();
            }
        }

        animationId = requestAnimationFrame(mainLoop);
    }
}

module.exports = CanvasLooper;