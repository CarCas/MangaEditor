module MangaEditor

open System.Windows.Forms
open System
open System.Drawing
open System.Drawing.Drawing2D
open LWC
open CommonTypes
open Shapes
open TextInBalloon
open Balloons
open Cartoon
open Paper

let f1 = new Form(Text="Manga Editor", Size=Size(500, 400))

let addBtn = new Button(Dock=DockStyle.Left, Text="Start creating a new Cartoon!")
f1.Controls.Add(addBtn)

let mutable boolSave = false
let mutable f = new Panel(Text="Paper", Dock=DockStyle.Fill, Visible=false)
f1.Controls.Add(f)

addBtn.Click.Add(fun _ ->
  f.Visible <- true
  f.Show()
  addBtn.Visible <- false
  let e = new Paper(Dock=DockStyle.Fill)

  let butEndPaper = new Button(Text="Finished", Location=Point(0, 115), Size=Size(120, 50))
  let butSave = new Button(Text="Save", Location=Point(0, 115), Size=Size(120, 50))
  let butEnd = new Button(Text="Close", Location=Point(0, 165), Size=Size(120, 50))
  let gbOpt = new GroupBox(Text="Options", Location=Point(0, 15), Size=Size(120, 100))
  let rbCreation = new RadioButton(Text="Create", Location=Point(5, 30), Size=Size(100, 20))
  let rbSelection = new RadioButton(Text="Select", Location=Point(5, 60), Size=Size(100, 20))

  rbCreation.CheckedChanged.Add(fun _ ->
    e.Creation <- true
    e.Selection <- false
  )

  rbSelection.CheckedChanged.Add(fun _ ->
    e.Creation <- false
    e.Selection <- true
  )

  gbOpt.Controls.Add(rbCreation)
  gbOpt.Controls.Add(rbSelection)

  butEndPaper.Click.Add(fun _ ->
    e.Creation <- false
    e.Selection <- false
    rbCreation.Checked <- false
    rbSelection.Checked <- false
    gbOpt.Visible <- false
    e.EndPaper <- true
    butEndPaper.Visible <- false
  )

  butSave.Click.Add(fun _ ->
    e.Save()
    boolSave <- true
  )

  butEnd.Click.Add(fun _ ->
    if not boolSave then
      let mb = MessageBox.Show("Do you wanna close without saving?", "Wait ...",
                MessageBoxButtons.YesNo, MessageBoxIcon.Question)
      if mb = DialogResult.Yes then
        boolSave <- true
        f.Visible <- false
        f.Dispose()
        f <- new Panel(Text="Paper", Dock=DockStyle.Fill, Visible=false)
        f1.Controls.Add(f)
        Application.DoEvents()
        addBtn.Visible <- true
    else
      f.Visible <- false
      f.Dispose()
      f <- new Panel(Text="Paper", Dock=DockStyle.Fill, Visible=false)
      f1.Controls.Add(f)
      Application.DoEvents()
      addBtn.Visible <- true
  )

  f.Controls.Add(gbOpt)
  f.Controls.Add(butEndPaper)
  f.Controls.Add(butEnd)
  f.Controls.Add(butSave)
  f.Controls.Add(e)

  e.Focus() |> ignore

)

f1.FormClosing.Add(fun _ ->
  if not boolSave then
    let mb = MessageBox.Show("Do you wanna close without saving?", "Wait ...",
              MessageBoxButtons.YesNo, MessageBoxIcon.Question)
    if mb = DialogResult.Yes then
      boolSave <- true
      Environment.Exit(0)
  else
    Environment.Exit(0)
)

f1.FormClosed.Add(fun _ -> 
  f1.Dispose()
)

f1.Show()

[<System.STAThread>]
do
  while true do
    Application.DoEvents()
  Application.Run(f1)
  