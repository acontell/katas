const draw = require('./pencil').draw;

function CanvasGameActions(ctx, height, width, squareSide) {
    let animationId;
    let keepGoing = true;

    this.cancel = () => {
        keepGoing = false;
        cancelAnimationFrame(animationId);
    };

    this.start = (tickFnc, interval) => {
        function mainLoop() {
            ctx.clearRect(0, 0, height, width);
            draw(ctx, tickFnc(), squareSide);
            if (keepGoing) {
                setTimeout(() => animationId = requestAnimationFrame(mainLoop), interval);
            }
        }

        animationId = requestAnimationFrame(mainLoop);
    }
}

module.exports = CanvasGameActions;