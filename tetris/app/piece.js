const _ = require('lodash');

function Piece(blocks, id) {
    this.getBlocks = () => blocks;
    this.getId = () => id;
    this.moveDown = () => _.forEach(blocks, block => block.moveDown());
    this.getLowestBlock = () => _.last(sortByRowAscColumnDesc());
    this.getHighestBlock = () => _.head(sortByRowAscColumnDesc());
    this.isEmpty = () => _.size(blocks) === 0;
    this.clearBlocks = lines => {
        blocks = _.filter(blocks, block => !_.includes(lines, block.getRow()));
        return this;
    };
    this.collapse = lines => {
        blocks = _.map(blocks, block => block.updatePosition(lines));
        return this;
    };

    function sortByRowAscColumnDesc() {
        return _.sortBy(blocks, block => block.getRow(), block => -block.getColumn());
    }
}

module.exports = Piece;