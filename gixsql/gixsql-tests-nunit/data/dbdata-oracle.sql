BEGIN
    BEGIN
        EXECUTE IMMEDIATE 'DROP TABLE emptable';

    EXCEPTION
         WHEN OTHERS THEN
         DBMS_OUTPUT.PUT_LINE(sqlcode);
                IF SQLCODE != -942 THEN
                     RAISE;
                END IF;
    END;       
   
    EXECUTE IMMEDIATE 'CREATE TABLE emptable ( eno integer, lname varchar(10), fname varchar(10), street varchar(20), city varchar(15), st varchar(2), zip varchar(5), dept varchar(4), payrate decimal(13,2), com numeric(3,2), miscdata varchar(128), dnum1 numeric(4,2), dnum2 numeric(4,2), dnum3 numeric(4,2) )';

    EXECUTE IMMEDIATE 'INSERT INTO emptable VALUES (123, ''Doe'', ''John'', ''123, Nowhere Lane'', ''Noplace'', ''NA'', ''00100'', ''DEP1'', 200.00, 1.23, ''abcd1234'', 00.00, 22.33, 33.44)';
    EXECUTE IMMEDIATE 'INSERT INTO emptable VALUES (456, ''Smith'', ''Jane'', ''456, Someplace Rd.'', ''Somewhere'', ''NA'', ''00111'', ''DEP2'', 130.00, 3.45, ''defg5678hijk'', 55.66, 00.00, 99.11)';
    EXECUTE IMMEDIATE 'INSERT INTO emptable VALUES (789, ''Smith2'', ''Jane2'', ''789, Someplace 2 Rd.'', ''Someplace Else'', ''NA'', ''00123'', ''DEP3'', 230.00, 4.56, ''98005678hijk'', 12.34, 45.67, 00.00)';


END;
