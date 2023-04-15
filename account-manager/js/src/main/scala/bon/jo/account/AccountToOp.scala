package bon.jo.account
import scalajs.js
import bon.jo.common.SimpleDate
import scalajs.js.JSConverters.*
object AccountToOp :
  type ComputedData =  IndexedSeq[((Int, Int), (js.Array[Operation[SimpleDate]], js.Array[Operation[SimpleDate]], Int, Int))]
  type ComputedDataRow =  ((Int, Int), (js.Array[Operation[SimpleDate]], js.Array[Operation[SimpleDate]], Int, Int))

  def apply(data: js.Array[AccountDto]):ComputedData = 
    val ops = data
      .map(e => e.id -> e)
      .toMap
      .values
      .map { e =>
        Operation(
          (e.amount * 100).toInt.cents,
          SimpleDate(
            e.date.getDate().toInt,
            e.date.getMonth().toInt + 1,
            e.date.getFullYear().toInt
          ),
          e.lib.trim(),
          e.libType.trim()
        )
      }
      .toJSArray

    val groupByMonth = ops.groupBy(e => e.date.year -> e.date.month)
    val seq = groupByMonth.toIndexedSeq
    val srted = seq.sortBy(e => e._1)
    srted.map { (k, v) =>

      val (debit, credit) = v.partition(_.value.value < 0)
      val debitSumCents = debit.map(_.value.value).sum
      val creditSumCents = credit.map(_.value.value).sum
      val debitSum = debitSumCents / 100d
      val creditSum = creditSumCents / 100d
      k -> (debit, credit, debitSumCents, creditSumCents)
    }

