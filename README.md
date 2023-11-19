The [fly.io distributed systems challenges](https://fly.io/dist-sys/) solved in Haskell.
Uses [maelstrom](https://github.com/jepsen-io/maelstrom/tree/main) as the workbench for running test workloads on our Haskell programs.

## Running
Running the programs through `make` has a dependency on `docker`.
Once the test workload has completed, results will be available on `stdout` as well as hosted on `localhost:20000`.

## Programs
### Echo.hs
A simple server that takes in an echo message and writes the same message back.

### Unique IDs
A simple server that generantes unique IDs based on a request and returns a unique ID.
The unique ID generation is using a naive approach in which it assumes that the message ID for each request is unique.
The unique ID generated will then take the form of `${node_id}${message_id}` where `node_id` is obtained from the `init` message sent from the maelstrom workbench.

### Broadcast
A simple server that gossips messages between all nodes in the cluster using a simple implementation of the gossip protocol.
The implementation details are described below.

The node runs three threads:
  - a listen thread
  - a gossip timer thread
  - an event thread

The listen thread handles messages received from `stdin` and generates a `MessageReceived` event for the event thread to handle.
This event thread handles events similar to other workloads.

The gossip thread triggers a `GossipTriggered` event every `500ms`.
The purpose of this thread is to aid in the broadcast of messages to other nodes as well as a way to ensure that nodes in a failed network partition ultimately are able to achieve eventual consistency.
When a node has no messages, there is no point in sending a gossip message so empty gossip messages are omitted.
Additionally, sending a gossip message every `500ms` results in a high latency for nodes that are several hops away from the source node.
Therefore, upon every gossip message received, the node will re-send the gossip message to its neighbours.
In order to ensure that minimal amount of messages are sent across the network, a condition is added so that gossip messages are only sent in bulk (i.e. when there are 3 broadcast payloads that it hasn't seen before).

