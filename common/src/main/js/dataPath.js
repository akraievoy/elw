   function initInstructions() {
      document.DataPath.initInstructions();
   }

   function zeroRegisters() {
     document.DataPath.zeroRegisters();
   }

   function resetData() {
     document.DataPath.resetData();
   }

   function resetDataPath(){
      document.DataPath.resetDataPath();
   }

   function setInstruction(instruction, address) {
      document.DataPath.setInstruction(instruction, address);
   }

   function setRegister(word, registerNumber) {
     document.DataPath.setRegister(word, registerNumber);
   }
  
   function setData(dataItem, address) {
     document.DataPath.setData(dataItem, address);
   }

   function returnedDataAccess(value, address, access) {
     parent.returnedDataAccess(value, address, access);
   }

   function returnedRegisterAccess(read1, read2, write, value) {
     parent.returnedRegisterAccess(read1, read2, write, value);
   }

   function returnedInstructionAccess(address) {
     parent.returnedInstructionAccess(address);
   }

   function displayMessage(message) {
      alert("" + message);
   }
