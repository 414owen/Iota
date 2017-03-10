const WebSocket = require('ws');

const port = 8001;
const wss = new WebSocket.Server({ port: port });
console.log("Listening on port", port);

// Broadcast to all.
wss.broadcast = function broadcast(data) {
	wss.clients.forEach(function each(client) {
		if (client.readyState === WebSocket.OPEN) {
			client.send(data);
		}
	});
};

wss.on('connection', function connection(ws) {
	console.log("Client connected");
	ws.on('message', function incoming(data) {
		console.log("Recieved message");
		wss.broadcast(data);
	});
});
