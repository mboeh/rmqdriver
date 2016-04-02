# amqptap

Interactive tool for watching messages on RabbitMQ. Under development, partly as a learning project.

## Usage

Batch usage, with the `jq` JSON filter and pretty-printer:

```
% cat > batch.amqptap
add foo
bind foo amq.topic foo.bar
bind foo amq.topic bar.baz
tail foo json
^D
% amqptap batch.amqptap | jq .message
{
  "message": "foo"
}
{
  "message": "bar"
}
^C
%
```

Inline batch usage:

```
% amqptap -e "tap foo amq.topic foo.* json"
```

Interactive usage:

```
% amqptap
> add foo
added
> bind foo amq.topic foo.bar
bound
> bind foo amq.topic foo.bar.baz
bound
> unbind foo amq.topic foo.bar
unbound
> list
Queue "foo"
> drain foo default
[messages print to stdout]
drained
```

## Commands

* `add`: Create a named, temporary (exclusive) queue that will be destroyed when the connection is ended.
* `bind QUEUE EXCHANGE ROUTINGKEY`: Bind the queue to the named exchange with the given routing key.
* `unbind QUEUE EXCHANGE ROUTINGKEY`: Unbind the queue.
* `list`: List created queues.
* `drain QUEUE SINK`: Consume all messages currently on QUEUE into SINK. `text` and `json` sinks are available.
* `tail QUEUE SINK`: Start consuming messages on QUEUE into SINK. Stops with Ctrl-C.
* `tap QUEUE EXCHANGE ROUTINGKEY SINK`: `add`, `bind`, and `tail` combined.

## Todo

- [x] message formatting
- [x] better interface (batch, command line)
- [x] batch mode -- load a script or scripts on start, or only run a script
- [ ] `purge`
- [ ] better output for `list`
