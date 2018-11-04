let _ = require('lodash');

function isPositionTaken(block, blocks) {
    return blocks.some(aBlock => block.equals(aBlock));
}

module.exports = {
    checkHasNotReachedBottom: (lowestBlock, bottom) => lowestBlock < bottom,
    checkHasNotReachedRightSide: (farRightBlockColumn, bottom) => farRightBlockColumn < bottom,
    checkHasNotReachedLeftSide: farLeftBlockColumn => farLeftBlockColumn >= 0,
    checkNoCollision: (blocks, allBlocks) => _.every(blocks, block => !isPositionTaken(block, allBlocks))
};