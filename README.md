# amqptap

Interactive tool for watching messages on RabbitMQ. Under development, partly as a learning project.

## Usage

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
* `drain QUEUE SINK`: Consume all messages currently on QUEUE into SINK. Currently the only sink available, `default`, goes to stdout.
* `tail QUEUE SINK`: Start consuming messages on QUEUE into SINK. Stops with Ctrl-C.

## Todo

- [ ] `undrain`
- [ ] better-behaved background stdout drain
- [ ] drain to file: `drain foo file:foo.log`
- [ ] message formatting
- [ ] better interface
- [x] batch mode -- load a script or scripts on start, or only run a script
- [ ] `purge`
- [ ] better output for `list`
