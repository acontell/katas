module.exports = (document, conf) => {
    const ctx = document.getElementById(conf.canvasId).getContext('2d');
    ctx.canvas.width = conf.width;
    ctx.canvas.height = conf.height;
    return ctx;
};