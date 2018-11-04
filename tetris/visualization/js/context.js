module.exports = (width, height) => {
    const ctx = document.getElementById('canvas').getContext('2d');
    ctx.canvas.width = width;
    ctx.canvas.height = height;
    return ctx;
};