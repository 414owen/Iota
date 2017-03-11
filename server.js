#!/usr/bin/env node
var ws = require('ws');

function drawServ(wss) {
	var lastPoint = "100,100";
	const valid = /^-?\d+,-?\d+$/;

	// Broadcast to all
	wss.broadcast = function broadcast(data) {
		wss.clients.forEach(function each(client) {
			if (client.readyState === ws.OPEN) {
				client.send(data);
			}
		});
	};

	// On message recieved
	function incoming(data) {
		if (valid.test(data)) {
			console.log("Recieved", data);
			wss.broadcast(data);
			lastPoint = data;
		}
	}

	// On new connection
	wss.on('connection', function connection(ws) {
		console.log("Client connected");
		ws.send(lastPoint);
		ws.on('message', incoming);
	});
}
module.exports = drawServ;
