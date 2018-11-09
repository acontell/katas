module.exports = (width, height, canvasId) => {
    const ctx = document.getElementById(canvasId).getContext('2d');
    ctx.canvas.width = width;
    ctx.canvas.height = height;
    return ctx;
};