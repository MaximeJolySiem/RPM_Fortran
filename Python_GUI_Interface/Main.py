import sys
import multiprocessing

if sys.version_info[0] < 3:
    from Tkinter import *
    import tkFileDialog as filedialog
else:
    from tkinter import *
    from tkinter import filedialog

vtkFilename = "Empty"
meshFilename = "Empty"
ParticleFilename = "Empty"
x_min = 0
x_max = 0
y_min = 0
y_max = 0
Delta = float(0)
Label_test_for_memory = 0

Get_Thread_Number = multiprocessing.cpu_count()

def get_entry_fields():
   
   text_file = open("../SNGM_config", "w")
   text_file.write("[meanflow]" + "\n")
   text_file.write("cfddatafile=" + vtkFilename + "\n")
   text_file.write("turb_Lmin=" + Lambda_limiter.get() + "\n")
   
   text_file.write("\n")
   text_file.write("[strengthint]" + "\n")
   if Evolving_turbulence.get():
       text_file.write("turb_evo_model=LangevinEwert" + "\n")
   else:
       text_file.write("turb_evo_model=frozen" + "\n")
   
   text_file.write("\n")
   text_file.write("[seeding]" + "\n")
   text_file.write("RanCstSeeds=" + str(Seed.get()) + "\n")
   text_file.write("Nparticles=" + Particle_Number.get() + "\n")
   
   text_file.write("\n")
   text_file.write("x_min=" + str(x_min) + "\n")
   text_file.write("x_max=" + str(x_max) + "\n")
   text_file.write("y_min=" + str(y_min) + "\n")
   text_file.write("y_max=" + str(y_max) + "\n")
   text_file.write("Delta=" + str(Delta) + "\n")
   
   text_file.write("\n") 
   text_file.write("[input]" + "\n")
   text_file.write("viz_interval=" + Acoustic_Time_Step.get() + "\n")
   text_file.write("end_time=" + Final_time.get() + "\n")
    
   text_file.write("\n") 
   text_file.write("[global_eval]" + "\n")
   text_file.write("amplitudeScalingParticleOrGrid=" + Scaling_type.get() + "\n")
   text_file.write("Rconst=" + Radius_consideration.get() + "\n")
   text_file.write("filter=" + Filter_type.get() + "\n")
   if float(Particle_Number.get()) > 0:
      text_file.write("partVol=" + str(((x_max-x_min)*(y_max-y_min)/float(Particle_Number.get()))**(.5)) + "\n")
   else :
      text_file.write("partVol=0" + "\n")
   text_file.write("Parallel_Number_Thread=" + str(Set_Number_Thread.get()) + "\n")

   text_file.write("\n") 
   text_file.write("[writing]" + "\n")
   text_file.write("write_Particle=" + str(Part_write.get()) + "\n")
   text_file.write("write_Vel=" + str(Vel_write.get()) + "\n")
   text_file.write("write_Vor=" + str(Vor_write.get()) + "\n")
   text_file.write("write_Lsum=" + str(Lsum_write.get()) + "\n")
   text_file.write("write_binary_format=" + str(bin_write.get()) + "\n")
   text_file.write("write_fourier=" + str(fourier_write.get()) + "\n")

   Freq_max = int(float(Freq_link.get()))   
   T = float(Final_time.get())
   dt = float(Acoustic_Time_Step.get())      
   if Freq_max > 1/(2*dt):
       Num_freq = T/(2*dt)
   else:
       Num_freq = int(round(T*Freq_max))

   text_file.write("Number_freq=" + str(int(Num_freq)) + "\n")

   text_file.write("\n") 
   text_file.write("[Save]\n")
   text_file.write("Enable_save="+ str(Save_part.get()) + "\n")
   text_file.write("Save_path=" + ParticleFilename + "\n")
   
   text_file.close()
   
 
    
   
   

  
   
   text_file = open("Save", "w")
   text_file.write(vtkFilename + "\n") #0
   text_file.write(str(x_min) + "\n") #1
   text_file.write(str(x_max) + "\n") #2
   text_file.write(str(y_min) + "\n") #3
   text_file.write(str(y_max) + "\n") #4
   text_file.write(str(Delta) + "\n") #5
   text_file.write(str(Seed.get()) + "\n") #6
   text_file.write(Particle_Number.get() + "\n") #7
   text_file.write(Lambda_limiter.get() + "\n") #8
   if Evolving_turbulence.get(): #9
       text_file.write("LangevinEwert" + "\n")
   else:
       text_file.write("frozen" + "\n")
   text_file.write(Acoustic_Time_Step.get() + "\n") #10
   text_file.write(Final_time.get() + "\n") #11
   text_file.write(Filter_type.get() + "\n") #12
   text_file.write(Scaling_type.get() + "\n") #13
   text_file.write(Radius_consideration.get() + "\n") #14
   text_file.write(str(Set_Number_Thread.get()) + "\n") #15
   text_file.write(str(bin_write.get()) + "\n") #16
   text_file.write(str(Part_write.get()) + "\n") #17
   text_file.write(str(Vel_write.get()) + "\n") #18
   text_file.write(str(Vor_write.get()) + "\n") #19
   text_file.write(str(Lsum_write.get()) + "\n") #20
   text_file.write(str(fourier_write.get()) + "\n") #21
   text_file.write(Freq_link.get() + "\n") #22
   text_file.write(str(Save_part.get()) + "\n") #23
   text_file.write(ParticleFilename + "\n") #24
   
   
   
   text_file.close()    
   
   
   
   
   
   
   
 
   
   
   
   
   
   
   
def openvtkfile():
    global vtkFilename
    vtkFilename = filedialog.askopenfilename(parent=Mafenetre)
    
    if len(vtkFilename) > 3:
       if vtkFilename[len(vtkFilename)-4:len(vtkFilename)] != '.vtk':
          PopUp = Tk()
          PopUp.geometry("300x75")
          PopUp.title('Error Vtk open file')
          Label(PopUp, text="Please insert valid vtk file", font='Helvetica 18 bold').pack()
          Button(PopUp, text='Close', command=PopUp.destroy).pack()
          PopUp.mainloop()
       else:
          Path_vtk.delete(0, 'end')
          Path_vtk.insert(0,vtkFilename)
          file = open(vtkFilename, "r") 
          
          data = file.readlines() 
          global x_min
          global x_max
          global y_min
          global y_max
          global Delta
          k = 0
          while data[k][0:2] != "DI":
              k+=1
          
          nx = ""
          ny = ""
          i = 0
          while data[k][i] != " ":
              i += 1
          i+=1
          while data[k][i] != " ":
              nx+=data[k][i]
              i += 1
          i+=1
          while data[k][i] != " ":
              ny+=data[k][i]
              i += 1
          
          i = 0
          while data[k+1][i] != " ":
              i += 1
          i+=1
          Delta = ""
          
          while data[k+1][i] != " ":
              Delta+=data[k+1][i]
              i += 1
              
          i = 0
          while data[k+2][i] != " ":
              i += 1
          i+=1
          x_min = ""
          y_min = ""
          
          while data[k+2][i] != " ":
              x_min+=data[k+2][i]
              i += 1
          i+=1
          while data[k+2][i] != " ":
              y_min+=data[k+2][i]
              i += 1
          
          Delta = float(Delta)
          x_min = float(x_min)
          y_min = float(y_min)
          nx = int(nx)
          ny = int(ny)
          x_max = x_min+(nx-1)*Delta
          y_max = y_min+(ny-1)*Delta
          Label(Mafenetre, text="x_min = " + str(x_min)).grid(row=3, column=0, sticky=W)
          Label(Mafenetre, text="y_min = " + str(y_min)).grid(row=4, column=0, sticky=W)
          Label(Mafenetre, text="x_max = " + str(x_max)).grid(row=3, column=0, sticky=W, padx = 100)
          Label(Mafenetre, text="y_max = " + str(y_max)).grid(row=4, column=0, sticky=W, padx = 100)
          Label(Mafenetre, text="Mesh size = " + str(Delta)).grid(row=5, column=0, sticky=W)
   
    




def openParticlefile():
    global ParticleFilename
    ParticleFilename = filedialog.askopenfilename(parent=Mafenetre)
    print(ParticleFilename)
    
    if len(ParticleFilename) > 4:
        Path_particle.delete(0, 'end')
        Path_particle.insert(0,ParticleFilename)

    
 

def EstimateMemory():
   dt = float(Acoustic_Time_Step.get())
   T = float(Final_time.get())
   Nt = int(round(T/dt))
   nx = int(round((x_max-x_min+Delta)/Delta))
   ny = int(round((y_max-y_min+Delta)/Delta))
   Freq_max = int(float(Freq_link.get()))
   if Freq_max > 1/(2*dt):
       Num_freq = T/(2*dt)
   else:
       Num_freq = int(round(T*Freq_max))

   Nb_variable = 0
   Memory_estimation = 0
   
   if (bin_write.get() == 1):
       if Vel_write.get() == 1:
          Memory_estimation = Memory_estimation+nx*ny*16*Nt
       if Vor_write.get() == 1:
          Memory_estimation = Memory_estimation+nx*ny*12*Nt
       if Lsum_write.get() == 1:
          Memory_estimation = Memory_estimation+nx*ny*16*Nt
       if fourier_write.get() == 1:
          Memory_estimation = Memory_estimation+nx*ny*8*Num_freq*2
   else:
       if Vel_write.get() == 1:
          Memory_estimation = Memory_estimation+nx*ny*35*Nt
       if Vor_write.get() == 1:
          Memory_estimation = Memory_estimation+nx*ny*18*Nt
       if Lsum_write.get() == 1:
          Memory_estimation = Memory_estimation+nx*ny*35*Nt
       if fourier_write.get() == 1:
          Memory_estimation = Memory_estimation+nx*ny*36*Num_freq*2
       
       
   if Part_write.get() == 1:
      if (bin_write.get() == 1):
          Memory_estimation = Memory_estimation + int(Particle_Number.get())*3*12*Nt
      else:
          Memory_estimation = Memory_estimation + int(Particle_Number.get())*3*18*Nt
      
   Memory_estimation = int(round(Memory_estimation*1e-6))
   Memory_string_var.set(str(Memory_estimation) + ' Mo')
 
 
 
    
def EstimateRAMMemory():
   dt = float(Acoustic_Time_Step.get())
   T = float(Final_time.get())
   Nt = int(round(T/dt))
   nx = int(round((x_max-x_min+Delta)/Delta))
   ny = int(round((y_max-y_min+Delta)/Delta))
   Freq_max = int(float(Freq_link.get()))
   if Freq_max > 1/(2*dt):
       Num_freq = T/(2*dt)
   else:
       Num_freq = int(round(T*Freq_max))

   Nb_variable = 0
   Memory_estimation = Num_freq * 16 * nx*ny * fourier_write.get()
   
   Memory_estimation = int(round(Memory_estimation*1e-6))
   RAM_Memory_string_var.set(str(Memory_estimation) + ' Mo')   
   
 
 
    
    
try:
 with open('Save'): 
     If_save = 1
     pass
except IOError:
    If_save = 0
    


if(If_save==1):
    file = open('Save', "r") 
    data_saved = file.readlines() 

   

Mafenetre = Tk()



Mafenetre.title('RPM Options')

Label(Mafenetre, text="Meanflow parameters", font='Helvetica 18 bold').grid(row=0, column=0, sticky=W)

Label(Mafenetre, text="Open vtk file : ").grid(row=1, column=0, sticky=W)
ButtonVtk = Button(Mafenetre, text="Directory", command=openvtkfile).grid(row=1,column=0, padx=100, sticky=W)
Path_vtk = Entry(Mafenetre)
Path_vtk.config(width=30)
Path_vtk.grid(row=2,column=0, sticky=W)
if(If_save == 1):
    Path_vtk.insert(0,data_saved[0][0:len(data_saved[0])-1])
    vtkFilename = data_saved[0][0:len(data_saved[0])-1]
    x_min = float(data_saved[1][0:len(data_saved[1])-1])
    x_max = float(data_saved[2][0:len(data_saved[2])-1])
    y_min = float(data_saved[3][0:len(data_saved[3])-1])
    y_max = float(data_saved[4][0:len(data_saved[4])-1])
    Delta = float(data_saved[5][0:len(data_saved[5])-1])
    Label(Mafenetre, text="x_min = " + str(x_min)).grid(row=3, column=0, sticky=W)
    Label(Mafenetre, text="y_min = " + str(y_min)).grid(row=4, column=0, sticky=W)
    Label(Mafenetre, text="x_max = " + str(x_max)).grid(row=3, column=0, sticky=W, padx = 100)
    Label(Mafenetre, text="y_max = " + str(y_max)).grid(row=4, column=0, sticky=W, padx = 100)
    Label(Mafenetre, text="Mesh size = " + str(Delta)).grid(row=5, column=0, sticky=W)
    
else:
    Path_vtk.insert(0,"Empty")






Label(Mafenetre, text="Particle parameters", font='Helvetica 18 bold').grid(row=0, column=1, sticky=W)

Seed = IntVar()
Checkbutton(Mafenetre, text="Constant seed", variable=Seed).grid(row=1, column = 1, sticky=W)
if(If_save == 1):
    Seed.set(int(data_saved[6][0:len(data_saved[6])-1]))
else:    
    Seed.set(0)


Label(Mafenetre, text="Particle number : ").grid(row=2, column=1, sticky=W)
Particle_Number = Entry(Mafenetre)
Particle_Number.config(width=7)
Particle_Number.grid(row=2, column=1, sticky=W, padx=144)
if(If_save == 1):
    Particle_Number.insert(0,data_saved[7][0:len(data_saved[7])-1])
else:    
    Particle_Number.insert(0,"0")

Label(Mafenetre, text="Lenght scale limiter : ").grid(row=3, column=1, sticky=W)
Lambda_limiter = Entry(Mafenetre)
Lambda_limiter.config(width=7)
Lambda_limiter.grid(row=3, column=1, sticky=W, padx=144)
if(If_save == 1):
    Lambda_limiter.insert(0,data_saved[8][0:len(data_saved[8])-1])
else:
    Lambda_limiter.insert(0,"0")








Label(Mafenetre, text="Time parameters", font='Helvetica 18 bold').grid(row=0, column=2, sticky=W)

Evolving_turbulence = IntVar()
Checkbutton(Mafenetre, text="Evolving turbulence", variable=Evolving_turbulence).grid(row=1, column = 2, sticky=W)
if(If_save == 1):
    if(data_saved[9][0:len(data_saved[9])-1] == "LangevinEwert"):
        Evolving_turbulence.set(1)

Label(Mafenetre, text="Acoustic time step : ").grid(row=2, column=2, sticky=W)
Acoustic_Time_Step = Entry(Mafenetre)
Acoustic_Time_Step.config(width=5)
Acoustic_Time_Step.grid(row=2, column=2, sticky=W,padx=137)
if(If_save == 1):
    Acoustic_Time_Step.insert(0,data_saved[10][0:len(data_saved[10])-1])
else:
    Acoustic_Time_Step.insert(0,"0")

Label(Mafenetre, text="Final time : ").grid(row=3, column=2, sticky=W)
Final_time = Entry(Mafenetre)
Final_time.config(width=5)
Final_time.grid(row=3, column=2, sticky=W,padx=137)
if(If_save == 1):
    Final_time.insert(0,data_saved[11][0:len(data_saved[11])-1])
else:
    Final_time.insert(0,"0")












Line_second_part = 10


Label(Mafenetre, text="Evaluation parameters", font='Helvetica 18 bold').grid(row=Line_second_part, column=0, sticky=W)

Label(Mafenetre, text="Filter type : ").grid(row=Line_second_part+1, column=0, sticky=W)
Filter_type = StringVar()
Radiobutton(Mafenetre, text="Gaussian", variable=Filter_type, value="Gaussian").grid(row=Line_second_part+2, column = 0, sticky=W)
Radiobutton(Mafenetre, text="Liepmann", variable=Filter_type, value="Liepmann").grid(row=Line_second_part+3, column = 0, sticky=W)
Radiobutton(Mafenetre, text="VonKarman", variable=Filter_type, value="VonKarman").grid(row=Line_second_part+4, column = 0, sticky=W)
if(If_save == 1):
    Filter_type.set(data_saved[12][0:len(data_saved[12])-1])
else:
    Filter_type.set("Gaussian")

Label(Mafenetre, text="Scaling type : ").grid(row=Line_second_part+5, column=0, sticky=W)
Scaling_type = StringVar()
Radiobutton(Mafenetre, text="Particle", variable=Scaling_type, value="particle").grid(row=Line_second_part+6, column = 0, sticky=W)
Radiobutton(Mafenetre, text="Grid-Particle", variable=Scaling_type, value="kGrid_lambdaParticle").grid(row=Line_second_part+7, column = 0, sticky=W)
if(If_save == 1):
    Scaling_type.set(data_saved[13][0:len(data_saved[13])-1])
else:
    Scaling_type.set("particle")

Label(Mafenetre, text="Radius of consideration : ").grid(row=Line_second_part+8, column=0, sticky=W)
Radius_consideration = Entry(Mafenetre)
Radius_consideration.config(width=4)
Radius_consideration.grid(row=Line_second_part+8, column=0, padx = 165)
if(If_save == 1):
    Radius_consideration.insert(0,data_saved[14][0:len(data_saved[14])-1])
else:
    Radius_consideration.insert(0,"0")



Label(Mafenetre, text="Select the number of thread : ").grid(row=Line_second_part+9, column=0, sticky=W)
Set_Number_Thread = IntVar()
Scale_Thread = Scale(Mafenetre, from_=1, to=Get_Thread_Number, orient=HORIZONTAL, variable = Set_Number_Thread).grid(row=Line_second_part+10, column=0, sticky=W)
if(If_save == 1):
    Set_Number_Thread.set(int(data_saved[15][0:len(data_saved[15])-1]))














Label(Mafenetre, text="Writing parameters", font='Helvetica 18 bold').grid(row=Line_second_part, column=1, sticky=W)


bin_write = IntVar()
Checkbutton(Mafenetre, text="Write in binary format", variable=bin_write).grid(row=Line_second_part+1, column = 1, sticky=W)
if(If_save == 1):
    bin_write.set(int(data_saved[16][0:len(data_saved[16])-1]))
else:
    bin_write.set(0)



Part_write = IntVar()
Checkbutton(Mafenetre, text="Particle", variable=Part_write).grid(row=Line_second_part+2, column = 1, sticky=W)
if(If_save == 1):
    Part_write.set(int(data_saved[17][0:len(data_saved[17])-1]))
else:
    Part_write.set(0)

Vel_write = IntVar()
Checkbutton(Mafenetre, text="Velocity", variable=Vel_write).grid(row=Line_second_part+3, column = 1, sticky=W)
if(If_save == 1):
    Vel_write.set(int(data_saved[18][0:len(data_saved[18])-1]))
else:
    Vel_write.set(0)

Vor_write = IntVar()
Checkbutton(Mafenetre, text="Vorticity", variable=Vor_write).grid(row=Line_second_part+4, column = 1, sticky=W)
if(If_save == 1):
    Vor_write.set(int(data_saved[19][0:len(data_saved[19])-1]))
else:
    Vor_write.set(0)

Lsum_write = IntVar()
Checkbutton(Mafenetre, text="Lamb", variable=Lsum_write).grid(row=Line_second_part+5, column = 1, sticky=W)
if(If_save == 1):
    Lsum_write.set(int(data_saved[20][0:len(data_saved[20])-1]))
else:
    Lsum_write.set(0)

fourier_write = IntVar()
Checkbutton(Mafenetre, text="Fourier transform", variable=fourier_write).grid(row=Line_second_part+6, column = 1, sticky=W)
if(If_save == 1):
    fourier_write.set(int(data_saved[21][0:len(data_saved[21])-1]))
else:
    fourier_write.set(0)




Label(Mafenetre, text="Choose maximal frequency : ").grid(row=Line_second_part+7, column=1, sticky=W)
Set_freq = IntVar()

Freq_link = Entry(Mafenetre, textvariable=Set_freq)
Freq_link.config(width=8)
Freq_link.grid(row=Line_second_part+8, column=1, sticky=W)

if(If_save == 1):
    Freq_link.insert(0,data_saved[22][0:len(data_saved[22])-2])

ButtonMemory = Button(Mafenetre, text="Memory estimation", command=EstimateMemory).grid(row=Line_second_part+9,column=1, sticky=W, pady = 5)
Memory_string_var = StringVar(value="")
depositLabel = Label(Mafenetre, textvariable=Memory_string_var).grid(row=Line_second_part+9,column=1, padx = 130) 

ButtonRAMMemory = Button(Mafenetre, text="RAM Memory estimation", command=EstimateRAMMemory).grid(row=Line_second_part+10,column=1, sticky=W, pady = 5)
RAM_Memory_string_var = StringVar(value="")
RAMdepositLabel = Label(Mafenetre, textvariable=RAM_Memory_string_var).grid(row=Line_second_part+10,column=1, padx = 170) 

















Label(Mafenetre, text="Save parameters", font='Helvetica 18 bold').grid(row=Line_second_part, column=2, sticky=W)

Save_part = IntVar()
Checkbutton(Mafenetre, text="Enable save", variable=Save_part).grid(row=Line_second_part+1, column = 2, sticky=W)
if(If_save == 1):
    Save_part.set(int(data_saved[23][0:len(data_saved[23])-1]))

Label(Mafenetre, text="Open particle file : ").grid(row=Line_second_part+2, column=2, sticky=W)
ButtonVtk = Button(Mafenetre, text="Directory", command=openParticlefile).grid(row=Line_second_part+2,column=2, padx=130, sticky=W)
Path_particle = Entry(Mafenetre)
Path_particle.config(width=20)
Path_particle.grid(row=Line_second_part+3,column=2, sticky=W)
if(If_save == 1):
    Path_particle.insert(0,data_saved[24][0:len(data_saved[24])-1])
    ParticleFilename = data_saved[24][0:len(data_saved[24])-1]
else:
    Path_particle.insert(0,"Empty")




Button(Mafenetre, text='Close', command=Mafenetre.destroy).grid(row=25, column=0, sticky=W)
Button(Mafenetre, text='Make File', command=get_entry_fields).grid(row=25, column=0, sticky=W, padx=50)


Mafenetre.mainloop()
