const _ = require('lodash');

function Scorer() {
    const backToBackPoints = 1200;
    const tetrisPoints = 800;
    const linePoints = 100;
    let score = 0;

    this.addPoints = (completedLines, isBoardEmpty) => score += getPoints(_.size(completedLines), isBoardEmpty);
    this.getScore = () => score;

    function getPoints(clearedLines, isBoardEmpty) {
        return isBackToBack(clearedLines, isBoardEmpty) ? backToBackPoints : getLinesPoints(clearedLines);
    }

    function isBackToBack(clearedLines, isBoardEmpty) {
        return isBoardEmpty && isTetris(clearedLines);
    }

    function isTetris(clearedLines) {
        return clearedLines === 4;
    }

    function getLinesPoints(clearedLines) {
        return isTetris(clearedLines) ? tetrisPoints : clearedLines * linePoints;
    }
}

module.exports = Scorer;