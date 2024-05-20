# Servidor Shiny Server Embrapa

Servidor Shiny-Server Embrapa arroz e Feijão.

# Sumário

<!--ts-->
   1. [Sumário](#Sumário)
   2. [Estrutura das pastas](#Estrutura-das-pastas)
   3. [Pré-requisitos](#Pré-requisitos)
   4. [Instalação](#Instalação)
<!--te-->

# Estrutura das pastas

Os arquivos para a criaçao do container foram dividos em quatro pastas.

| path  |  Descrição  |
| ------------------- | ------------------- |
|  /apps/ |  Aplicativos que serão colocados no servidor. |
|  /conf/ |  Pasta onde fica a configuração do Shiny-Server. |
|  /log/  |  Pasta onde irá ficar amarzenado o log do Servidor.
|  /postgres/ | Pasta onde fica armazenados os dados do banco da aplicação.
|  /scripts/ |  Scripts que serão usados na criaçao do container. |

# Pré-requisitos

Antes de começar, você precisa ter instalado em sua máquina as seguintes ferramentas: <br>

<!--ts-->
   1. [Instalar docker](#1-Docker)
   2. [Iniciar serviço docker](#2-Iniciar-serviço-docker)
<!--te-->

### 1. Docker
Caso você não tenha o Docker instalado, execute o comando abaixo de acordo com a sua distribuição. <br>

##### 1.1. Ubuntu
 ```
 sudo apt-get update
 ```
 ```
 sudo apt-get install \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg \
    lsb-release
 ```
 ```
 curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
 ```
 ```
 sudo apt-get update
 ```
 ```
 sudo apt-get install docker-ce docker-ce-cli containerd.io
 ```

##### 1.2. CentOS
 ```
 sudo yum install -y yum-utils
 ```
 ```
 sudo yum-config-manager \
    --add-repo \
    https://download.docker.com/linux/centos/docker-ce.repo
 ```
 ```
 sudo yum install docker-ce docker-ce-cli containerd.io
 ```

#### 2. Iniciar serviço docker

Depois que o docker estiver instalado, inicie o serviço com o seguinte comando:
```
 sudo systemctl start docker
```
 
# Instalação da aplicação

<!--ts-->
   1. [Clonar repositório](#1-Clonar-repositório)
   2. [build servidor](#2-Criar-build-servidor)
<!--te-->

#### 1. Clonar repositório
Escolha um diretorio no seu computador e execute o comando: <br>

```
git clone https://github.com/FelipheStival/fundacaoABC
```

Mude o diretório para a pasta base do repositório: <br>

```
cd shiny-server-fundacao
```

#### 2. Criar build e executar o servidor
Dentro da pasta do repositório, deve ser feita a build da imagem. Dentro da pasta do repositório, execute o seguinte comando:

```
docker compose up
```

Agora entre no endereço abaixo e verifique se irá aparecer a raiz do servidor:
```
localhost:5050
```
Caso aparece uma página como a abaixo, significa que o servidor foi configurado com sucesso.

![Alt text](servidor.png)

Agora basta acessar o endereço abaixo para ter acesso a aplicação.

```
localhost:5050/fundacaoabc
```