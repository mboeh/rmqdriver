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
draining
[messages print to stdout]
```

## Todo

- [ ] `undrain`
- [ ] better-behaved background stdout drain
- [ ] drain to file: `drain foo file:foo.log`
- [ ] message formatting
- [ ] better interface
- [ ] batch mode -- load a script or scripts on start, or only run a script
- [ ] foreground drain, `dump`
- [ ] `purge`
- [ ] better output for `list`
