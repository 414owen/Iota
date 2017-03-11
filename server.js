#!/usr/bin/env node
var ws = require('ws');

function drawServ(wss) {
	var lastPoint = "100,100";
	const numTest = /^\d+$/;

	function validPoint(p) {
		const parts = p.split(",").map(function(a) {return numTest.test(a);})
		return parts.length == 2 && parts[0] && parts[1];
	}

	// Broadcast to all
	wss.broadcast = function broadcast(data) {
		wss.clients.forEach(function each(client) {
			if (client.readyState === ws.OPEN) {
				client.send(data);
			}
		});
	};

	function incoming(data) {
		if (validPoint(data)) {
			console.log("Recieved", data);
			wss.broadcast(data);
			lastPoint = data;
		}
	}

	wss.on('connection', function connection(ws) {
		console.log("Client connected");
		ws.send(lastPoint);
		ws.on('message', incoming);
	});
}
module.exports = drawServ;
