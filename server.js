#!/usr/bin/env node
var ws = require('ws');
function drawServ(wss) {
	const maxPoints = 20;
	var lastPoints = ["100,100,ccc"];
	const valid = /^-?\d+,-?\d+,[0-9a-f]{3}$/;
	wss.broadcast = function broadcast(data) {
		wss.clients.forEach(function each(client) {
			if (client.readyState === ws.OPEN) {
				client.send(data);
			}
		});
	};
	function incoming(data) {
		if (valid.test(data)) {
			const point = "p" + data;
			lastPoints.push(point);
			if (lastPoints.length >= maxPoints) {
				lastPoints.splice(0, lastPoints.length - maxPoints);
			}
			wss.broadcast(point);
			lastPoint = data;
		}
	}
	function updateUsers() { wss.broadcast("u" + wss.clients.size); }
	wss.on('connection', function connection(ws) {
		for (var i in lastPoints) {
			ws.send(lastPoints[i]);
		}
		updateUsers();
		ws.on('message', incoming);
		ws.on('close', updateUsers);
	});
}
module.exports = drawServ;
