let _ = require('lodash');

function checkHasNotReachedBottom(lowestBlock, bottom) {
    return lowestBlock <= bottom;
}

function checkNoCollision(blocks, allBlocks) {
    return _.every(blocks, block => !isPositionTaken(block, allBlocks));
}

function isPositionTaken(block, blocks) {
    return _.some(blocks, aBlock => block.equals(aBlock));
}

function checkHasNotReachedRightSide(farRightBlockColumn, bottom) {
    return farRightBlockColumn <= bottom;
}

function checkHasNotReachedLeftSide(farLeftBlockColumn) {
    return farLeftBlockColumn >= 0;
}

module.exports = {
    checkHasNotReachedBottom: checkHasNotReachedBottom,
    checkNoCollision: checkNoCollision,
    checkHasNotReachedRightSide: checkHasNotReachedRightSide,
    checkHasNotReachedLeftSide: checkHasNotReachedLeftSide
};