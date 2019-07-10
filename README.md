# Introduction

This project intends to present the work analysis for "Análise Exploratória de Dados" class.

The ideia is Practing R using a real anonymized Czech bank transactions, account info, and loan records released for PKDD'99 Discovery Challenge.

See the final website report at: [https://ldaniel.github.io/R_Bank_Berka](https://ldaniel.github.io/R_Bank_Berka).

## Professor
- Gustavo Mirapalheta

## Authors / students
|Profile|Name|E-mail|
|---|---|---|
|<a href="https://github.com/DanielFCampos"><img src="https://avatars2.githubusercontent.com/u/31582602?s=460&v=4" title="DanielFCampos" width="80" height="80"></a>|Daniel Campos|[(daniel.ferraz.campos@gmail.com)](daniel.ferraz.campos@gmail.com)|
|<a href="https://github.com/ldaniel"><img src="https://avatars2.githubusercontent.com/u/205534?s=460&v=4" title="ldaniel" width="80" height="80"></a>|Leandro Daniel|[(contato@leandrodaniel.com)](contato@leandrodaniel.com)|
|<a href="https://github.com/RodriGonca"><img src="https://avatars2.githubusercontent.com/u/50252438?s=460&v=4" title="RodriGonca" width="80" height="80"></a>|Rodrigo Goncalves|[(rodrigo.goncalves@me.com)](rodrigo.goncalves@me.com)|
|<a href="https://github.com/ygorlima1"><img src="https://avatars2.githubusercontent.com/u/52429828?s=460&v=4" title="ygorlima1" width="80" height="80"></a>|Ygor Lima|[(ygor_redesocial@hotmail.com)](ygor_redesocial@hotmail.com)|

# About the data
Data from a real Czech bank. From 1999.

The data about the clients and their accounts consist of following relations:

* relation account (4500 objects in the file ACCOUNT.ASC) - each record describes static characteristics of an account,

* relation client (5369 objects in the file CLIENT.ASC) - each record describes characteristics of a client,

* relation disposition (5369 objects in the file DISP.ASC) - each record relates together a client with an account i.e. this relation describes the rights of clients to operate accounts,

* relation permanent order (6471 objects in the file ORDER.ASC) - each record describes characteristics of a payment order,

* relation transaction (1056320 objects in the file TRANS.ASC) - each record describes one transaction on an account,

* relation loan (682 objects in the file LOAN.ASC) - each record describes a loan granted for a given account,

* relation credit card (892 objects in the file CARD.ASC) - each record describes a credit card issued to an account,

* relation demographic data (77 objects in the file DISTRICT.ASC) - each record describes demographic characteristics of a district.

Each account has both static characteristics (e.g. date of creation, address of the branch) given in relation "account" and dynamic characteristics (e.g. payments debited or credited, balances) given in relations "permanent order" and "transaction". Relation "client" describes characteristics of persons who can manipulate with the accounts. One client can have more accounts, more clients can manipulate with single account; clients and accounts are related together in relation "disposition". Relations "loan" and "credit card" describe some services which the bank offers to its clients; more credit cards can be issued to an account, at most one loan can be granted for an account. Relation "demographic data" gives some publicly available information about the districts (e.g. the unemployment rate); additional information about the clients can be deduced from this.

Source:
*This database was prepared by Petr Berka and Marta Sochorova.*
