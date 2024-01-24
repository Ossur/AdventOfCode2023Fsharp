namespace advent

open System.Text.Json

module OssLib =
  let inline prenta x =
      float x 
      printfn $"{x}"
      
  let inline prentaJason x =
      printfn $"{JsonSerializer.Serialize x}"