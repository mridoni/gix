CREATE TABLE emptable (
	eno int4 NULL,
	lname varchar(10) NULL,
	fname varchar(10) NULL,
	street varchar(20) NULL,
	city varchar(15) NULL,
	st varchar(2) NULL,
	zip varchar(5) NULL,
	dept varchar(4) NULL,
	payrate numeric(13,2) NULL,
	com numeric(3,2) NULL,
	miscdata varchar(128) NULL
);


INSERT INTO emptable (eno,lname,fname,street,city,st,zip,dept,payrate,com,miscdata) VALUES 
(123,'Doe','John','123, Nowhere Lane','Noplace','NA','00100','DEP1',100.00,0.00, 'abcd1234'),
(456,'Smith','Jane','456, Someplace Rd.','Somewhere','NA','00111','DEP2',200.00,1.00, 'defg5678hijk')
;