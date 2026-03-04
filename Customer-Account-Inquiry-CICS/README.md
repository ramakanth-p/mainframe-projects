# Customer Account Inquiry System – CICS Online Application

This project demonstrates an end-to-end **CICS online customer inquiry transaction** built using **COBOL, CICS, BMS, VSAM and JCL**.

The application simulates a real-time banking style inquiry system where a user enters a Customer ID and customer details are retrieved from a VSAM KSDS file and displayed on a BMS screen.

---

## Transaction Start Screen

![Start](Screenshots/01_Start.png)

---

## Customer ID Not Found

![Not Found](Screenshots/02_Not_Found.png)

---

## Successful Customer Inquiry

![Success](Screenshots/03_Success.png)

---

## CEDF Execution Flow

This shows the actual CICS command flow during transaction execution.

### RECEIVE MAP
![Receive](Screenshots/CEDF_01_RECEIVE.png)

### READ VSAM FILE
![Read](Screenshots/CEDF_02_READ.png)

### SEND MAP
![Send](Screenshots/CEDF_03_SEND.png)

---

## CICS Resource Definitions Verified using CEMT

### Program Definition
![CEMT PROG](Screenshots/CEMT_01_PROG.png)

### File Definition
![CEMT FILE](Screenshots/CEMT_02_MAP.png)

### Mapset Definition
![CEMT MAP](Screenshots/CEMT_03_FILE.png)

---

## VSAM Cluster Creation using IDCAMS (DEFINE CLUSTER)

### DEFINE – Part 1
![DEFINE1](Screenshots/VSAM_DEFINE_01.png)

### DEFINE – Part 2
![DEFINE2](Screenshots/VSAM_DEFINE_02.png)

---

## VSAM Data Load using IDCAMS (REPRO)

### REPRO – Part 1
![REPRO1](Screenshots/VSAM_REPRO_01.png)

### REPRO – Part 2
![REPRO2](Screenshots/VSAM_REPRO_02.png)

---

## Technologies Used

- COBOL
- CICS
- BMS Maps
- VSAM KSDS
- JCL
- IDCAMS
- CEDF Debugging
- CEMT Resource Definitions
