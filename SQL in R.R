################################################# 
################################################ 
# Problem 2
library(RMySQL)
mydb <- dbConnect(MySQL(), user = "root", password = "Mancity1", host = "localhost")
dbSendQuery(mydb, "CREATE DATABASE week2;")
dbSendQuery(mydb, "USE week2;")

dbSendQuery(mydb, "CREATE TABLE Suppliers (sid int,
sname varchar(40),
address varchar(50),
primary key(sid)
);")

dbSendQuery(mydb, "CREATE TABLE Parts (pid int,
            pname varchar(40),
            color varchar(7),
            primary key(pid)
);")

dbSendQuery(mydb, "CREATE TABLE Catalog(
            sid int,
            pid int,
            cost numeric(4,2),
            primary key(sid, pid),
            foreign key (sid) references Suppliers (sid),
            foreign key (pid) references Parts (pid)
            
);")

dbSendQuery(mydb, "insert into Suppliers values ('1','Acme Widget Suppliers','1 Grub St., Potemkin Village, IL 61801');")
dbSendQuery(mydb, "insert into Suppliers  values ('2','Big Red Tool and Die','4 My Way, Bermuda Shorts, OR 90305');")
sup <- fetch(dbSendQuery(mydb, "SELECT * FROM Suppliers"))
sup
dbSendQuery(mydb, "insert into Parts  values ('1','Left Handed Bacon Stretcher Cover', 'Red');")
dbSendQuery(mydb, "insert into Parts  values ('2','Shifter End', 'Black');")
dbSendQuery(mydb, "insert into Parts  values ('3','Acme Widget Washer', 'Red');")
par <- fetch(dbSendQuery(mydb, "SELECT * FROM Parts"))
par

dbSendQuery(mydb, "insert into Catalog values ('1','1','0.50');")
dbSendQuery(mydb, "insert into Catalog values ('1','2','0.50');")
dbSendQuery(mydb, "insert into Catalog values ('1','3','11.70');")
dbSendQuery(mydb, "insert into Catalog values ('2','2','0.55');")
dbSendQuery(mydb, "insert into Catalog values ('2','3','7.95');")
cat <- fetch(dbSendQuery(mydb, "SELECT * FROM Catalog"))
cat

res <- fetch(dbSendQuery(mydb, "select sname, pname from (Suppliers natural join Catalog) join Parts using(pid);"))
res
################################################# 
