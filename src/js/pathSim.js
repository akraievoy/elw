   function getBrowserName() 
   {  var browserName = navigator.appName;
      return browserName;
   }

   function returnedDataAccess(value, address, access) {
     DataInput.returnedDataAccess(value, address, access);
   }

   function returnedRegisterAccess(read1, read2, write, value) {
     RegisterInput.returnedRegisterAccess(read1, read2, write, value);
   }

   function returnedInstructionAccess(address) {
     InstructionInput.returnedInstructionAccess(address);
   }
