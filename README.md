# Erlang Produtor Cosumidor (The Bounded Buffer Problem)

### To run the project you can either:

#### The easy way:

##### Make sure Bash scripts are runnable 
```
  sudo chmod +x *.bs
```
##### Run bash scripts (names are self-descriptive)
```
./compile.bs
./run.bs
```

#### The _I love typing in tty_ way

##### First enter in Erlang shell 
```
erl
```
##### Then copile the erl module into Beam's byte code
```
c(spawn).
c(state_manager).
c(state_renderer).
```
##### And lastly start the application by running
```
spawn:start().
```
