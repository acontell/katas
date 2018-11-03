const path = require('path');

module.exports = {
    entry: './html_visualization/js/main.js',
    output: {
        filename: 'main.js',
        path: path.resolve(__dirname, './html_visualization/dist')
    },
    mode: 'production'
};