const path = require('path');
const visualizationPath = './visualization';

module.exports = {
    entry: visualizationPath + '/js/main.js',
    output: {
        filename: 'main.js',
        path: path.resolve(__dirname, visualizationPath + '/dist')
    },
    mode: 'production'
};