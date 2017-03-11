#!/usr/bin/env node
var ws = require('ws');

function drawServ(wss) {
	var lastPoint = "100,100,ccc";
	const valid = /^-?\d+,-?\d+,[0-9a-f]{3}$/;

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
		// console.log("Recieved", data);
		if (valid.test(data)) {
			// console.log("Valid");
			wss.broadcast("p" + data);
			lastPoint = data;
		}
	}

	function updateUsers() { wss.broadcast("u" + wss.clients.size); }

	// On new connection
	wss.on('connection', function connection(ws) {
		// console.log("Client connected");
		ws.send("p" + lastPoint);
		updateUsers();
		ws.on('message', incoming);
		ws.on('close', updateUsers);
	});
}
module.exports = drawServ;
