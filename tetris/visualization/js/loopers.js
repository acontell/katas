function CanvasLooper(pencil, converter) {
    const convert = converter.convert;
    let animationId;
    let keepGoing = true;

    this.cancel = () => {
        console.log('GAME OVER!');
        keepGoing = false;
        cancelAnimationFrame(animationId);
    };

    this.start = (tickFnc, interval) => {
        function paint(game) {
            pencil.clear();
            pencil.drawMatrix(convert(game.getBoard()));
            pencil.drawSeparatingLine();
            pencil.drawScore(game.getScore());
        }

        function mainLoop() {
            if (keepGoing) {
                paint(tickFnc());
                setTimeout(() => animationId = requestAnimationFrame(mainLoop), interval);
            }
        }

        animationId = requestAnimationFrame(mainLoop);
    }
}

module.exports = CanvasLooper;