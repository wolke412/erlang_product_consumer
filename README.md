# Erlang Produtor Cosumidor (The Bounded Buffer Problem)


> Propor um sistema, que implemente, e simule a implementação de um sistema com dois (02) produtores, que produzem itens aleatoriamente diferentes (dois tipos) e outros quatro (04) consumidores que devem ter necessidade de consumir produtos aleatoriamente diferentes (entre os dois tipos), cada tipo destes produtos deve ter um tempo diferente para produção e o dobro do tempo para consumo, sendo respectivamente 3,5 e 7,5 segundos (tempo de produção e o dobro para consumo).

### Application Preview
!["Imagem exemplificando como o sistema será exibido em tela"](/preview.png)


# To run the project you can either:

* Go the easy way:

  - ##### Make sure Bash scripts are runnable 
  ```
    sudo chmod +x *.bs
  ```
  - ##### Run bash scripts (names are self-descriptive)
  ```
  ./compile.bs
  ./run.bs
  ```


* Go the _I love typing in tty_ way

  - ##### First enter the Erlang shell by typing
  ```
  erl
  ```
  - ##### Then copile the erlang modules into Beam's byte code
  ```erlang
  c(spawn).
  c(state_manager).
  c(state_renderer).
  ```
  - ##### Lastly, start the application by running
  ```erlang
  spawn:start().
  ```
