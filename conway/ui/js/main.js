const _ = require('lodash');
const conf = require('./configuration');
const Pencil = require('./pencil');
const buildContext = require('./context');
const ctx = buildContext(document, conf);
const pencil = new Pencil(ctx, conf.pencilConf);
const ws = new WebSocket(conf.ws);

ws.onmessage = ev => pencil.draw(JSON.parse(_.get(ev, 'data', [[]])));
