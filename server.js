#!/usr/bin/env node
var ws = require('ws');
function drawServ(wss) {
	const maxPoints = 60;
	const lastPoints = ["100,100,ccc"];
	const valid = /^-?\d+,-?\d+,[0-9a-f]{3}$/;
	wss.broadcast = function broadcast(wsEx, exclude, data) {
		wss.clients.forEach(function each(ws) {
			if (ws.readyState === ws.OPEN && (!exclude || ws !== wsEx)) {
				ws.send(data);
			}
		});
	};
	function incoming(broadcastExclusive, data) {
		if (valid.test(data)) {
			const point = "p" + data;
			lastPoints.push(point);
			if (lastPoints.length >= maxPoints) {
				lastPoints.splice(0, lastPoints.length - maxPoints);
			}
			broadcastExclusive(point);
		}
	}
	function updateUsers() {wss.broadcast(null, false, "u" + wss.clients.size);}
	wss.on('connection', function connection(ws) {
		for (var i in lastPoints) {ws.send(lastPoints[i]);}
		updateUsers();
		ws.on('message', incoming.bind(null, wss.broadcast.bind(null, ws, true)));
		ws.on('close', updateUsers);
	});
}
module.exports = drawServ;
