

//import './account.js'
class Op{
  date;
  amount;
  lib;
  libType;
  id;
  constructor(e){
    this.amount = e.montant;
    this.lib = e.libelleOperation;
    this.libType = e.libelleTypeOperation;
    this.date  =new Date(e.dateValeur);
    this.id = e.fitid;
  }

  getDateFomatted(){
    return `${this.date.getDate()}/${this.date.getMonth()+1}/${this.date.getFullYear()}` 
  }
}
const clean1 = account.map(e =>new Op(e));

Account.load(clean1);
