const _ = require('lodash');

module.exports = {
    sortByRowAscColumnDesc: blocks => _.sortBy(blocks, block => block.getRow(), block => -block.getColumn()),
    sortByColumnAsc: blocks => _.sortBy(blocks, block => block.getColumn())
};