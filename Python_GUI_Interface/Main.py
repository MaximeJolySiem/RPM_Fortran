#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Aug  1 18:05:54 2018

@author: maxime
"""

from Tkinter import *
import tkFileDialog as filedialog

vtkFilename = ""
meshFilename = ""
x_min = 0
x_max = 0
y_min = 0
y_max = 0
Delta = 0

def get_entry_fields():
   
   text_file = open("SNGM_config", "w")
   text_file.write("[meanflow]" + "\n")
   text_file.write("cfddatafile=" + vtkFilename + "\n")
   text_file.write("cfdmeshfile=" + meshFilename +  "\n")
   text_file.write("format=vtk" + "\n")
   text_file.write("turb_C0=2.1" + "\n")
   text_file.write("beta_star=0.09" + "\n")
   text_file.write("turb_cl=0.54" + "\n")
   text_file.write("turb_Cmu=0.09" + "\n")
   text_file.write("turb_Lmax=100" + "\n")
   
   text_file.write("\n")
   text_file.write("[particleint]" + "\n")
   text_file.write("TIM=RK11-Car" + "\n")
   
   text_file.write("\n")
   text_file.write("[strengthint]" + "\n")
   text_file.write("TIM=RK11-Car" + "\n")
   if Evolving_turbulence.get():
       text_file.write("turb_evo_model=LangevinEwert" + "\n")
   else:
       text_file.write("turb_evo_model=frozen" + "\n")
   
   text_file.write("\n")
   text_file.write("[seeding]" + "\n")
   text_file.write("cumulativeDistributionFunction=1" + "\n")
   text_file.write("RanCstSeeds=" + str(Seed.get()) + "\n")
   text_file.write("Nparticles=" + Particle_Number.get() + "\n")
   text_file.write("VelocityReference=1e300" + "\n")
   
   text_file.write("\n")
   text_file.write("x_min=" + str(x_min) + "\n")
   text_file.write("x_max=" + str(x_max) + "\n")
   text_file.write("y_min=" + str(y_min) + "\n")
   text_file.write("y_max=" + str(y_max) + "\n")
   text_file.write("y_min2=" + str(y_min) + "\n")
   text_file.write("y_max2=" + str(y_max) + "\n")
   text_file.write("z_min=0" + "\n")
   text_file.write("z_max=0" + "\n")
   
   text_file.write("\n") 
   text_file.write("init_x_min=" + str(x_min) + "\n")
   text_file.write("init_x_max=" + str(x_max) + "\n")
   text_file.write("init_y_min=" + str(y_min) + "\n")
   text_file.write("init_y_max=" + str(y_max) + "\n")
   text_file.write("init_z_min=0" + "\n")
   text_file.write("init_z_max=0" + "\n")
   text_file.write("sink_coord_right=" + str(x_max) + "\n")
   text_file.write("sink_coord_top=" + str(y_max) + "\n")
   text_file.write("sink_coord_bottom=" + str(y_min) + "\n")
   
   text_file.write("\n") 
   text_file.write("[input]" + "\n")
   text_file.write("timestep=" + RPM_Time_Step.get() + "\n")
   text_file.write("begin_time=0.0" + "\n")
   text_file.write("end_time=" + Final_time.get() + "\n")
   
   text_file.write("\n") 
   text_file.write("[output]" + "\n")
   text_file.write("SNGM_output=1" + "\n")
   text_file.write("outputdir=output" + "\n")
   text_file.write("meanfile=preCalc_valeoCD.dat" + "\n")
   text_file.write("progress_treshold=1" + "\n")
   text_file.write("viz_interval=" + Acoustic_Time_Step.get() + "\n")
   text_file.write("output_particles=1" + "\n")
   text_file.write("particle_file=pF" + "\n")
   text_file.write("turbulence_file=pseudo" + "\n")
   text_file.write("viz_meanflow=1" + "\n")
    
   text_file.write("\n") 
   text_file.write("mpfile=" + "\n")
   text_file.write("mpset=" + "\n")
   text_file.write("mpset_MPposFile=" + "\n")
   text_file.write("mpformat=" + "\n")
   text_file.write("mp_interval=" + "\n")
   text_file.write("mp_var=" + "\n")
    
   text_file.write("\n") 
   text_file.write("[global_eval]" + "\n")
   text_file.write("scalingLocalOrGlobal=global" + "\n")
   text_file.write("amplitudeScalingParticleOrGrid=" + Scaling_type.get() + "\n")
   text_file.write("convectionOnly=0" + "\n")
   text_file.write("Rconst=" + Radius_consideration.get() + "\n")
   text_file.write("vorticity=1" + "\n")
   text_file.write("velocity=1" + "\n")
   text_file.write("filter=" + Filter_type.get() + "\n")
   if float(Particle_Number.get()) > 0:
      text_file.write("partVol=" + str(((x_max-x_min)*(y_max-y_min)/float(Particle_Number.get()))**(.5)) + "\n")
   else :
      text_file.write("partVol=0" + "\n")
   text_file.write("use_intermesh=0" + "\n")
   text_file.write("x_minthreshold=" + str(x_min) + "\n")
   text_file.write("x_threshold=" + str(x_max) + "\n")
   text_file.write("y_minthreshold=" + str(y_min) + "\n")
   text_file.write("y_threshold=" + str(y_max) + "\n")
   text_file.write("min_lambda_particle=" + Lambda_limiter.get() + "\n")
    
   text_file.write("\n") 
   text_file.write("[source_calc]" + "\n")
   text_file.write("calc_Lu=1" + "\n")
   text_file.write("calc_Lom=1" + "\n")
   text_file.write("calc_L2=1" + "\n")
   text_file.write("calc_Sum=1" + "\n")
    
   text_file.write("\n") 
   text_file.write("[window]" + "\n")
   text_file.write("Lx1=0.05" + "\n")
   text_file.write("Lx2=0.1" + "\n")
   text_file.write("Ly1=0.04" + "\n")
   text_file.write("Ly2=0.06" + "\n")
   text_file.write("x1=-0.15" + "\n")
   text_file.write("x2=0.45" + "\n")
   text_file.write("y1=-0.08" + "\n")
   text_file.write("y2=0.12" + "\n")
    
   text_file.write("\n") 
   text_file.write("[interpolation]" + "\n")
   text_file.write("radiusfactor=0.064" + "\n")
   text_file.write("mappingcoef1=1.0e+00;" + "\n")
   text_file.write("mappingcoef2=4.0e+00;" + "\n")
    
   text_file.write("\n") 
   text_file.write("[statistics]" + "\n")
   text_file.write("calc_velmean=0" + "\n")
   text_file.write("calc_velsqrmean=0" + "\n")
   text_file.write("calc_velcrossmean=0" + "\n")
   text_file.write("beginComputingStatistics=0" + "\n")
   text_file.write("beginWritingStatistics=0" + "\n")
   
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
   
    
    



def openmeshfile():
    global meshFilename
    meshFilename = filedialog.askopenfilename(parent=Mafenetre)
    
    if len(meshFilename) > 4:
       if meshFilename[len(meshFilename)-5:len(meshFilename)] != '.mesh':
          PopUp = Tk()
          PopUp.geometry("350x75")
          PopUp.title('Error Mesh open file')
          Label(PopUp, text="Please insert valid mesh file", font='Helvetica 18 bold').pack()
          Button(PopUp, text='Close', command=PopUp.destroy).pack()
          PopUp.mainloop()
       else:
          Path_mesh.delete(0, 'end')
          Path_mesh.insert(0,meshFilename)


    
   
   
# Création de la fenêtre principale (main window)
Mafenetre = Tk()



Mafenetre.title('RPM Options')

Label(Mafenetre, text="Meanflow parameters", font='Helvetica 18 bold').grid(row=0, column=0, sticky=W)
Label(Mafenetre, text="Open vtk file : ").grid(row=1, column=0, sticky=W)
ButtonVtk = Button(Mafenetre, text="Directory", command=openvtkfile).grid(row=1,column=0, padx=100, sticky=W)
Path_vtk = Entry(Mafenetre)
Path_vtk.config(width=20)
Path_vtk.grid(row=2,column=0, sticky=W)
Path_vtk.insert(0,"Empty")

Label(Mafenetre, text="Open mesh file : ").grid(row=6, column=0, sticky=W)
ButtonMesh = Button(Mafenetre, text="Directory", command=openmeshfile).grid(row=6,column=0, padx=100, sticky=W)
Path_mesh = Entry(Mafenetre)
Path_mesh.config(width=20)
Path_mesh.grid(row=7,column=0, sticky=W)
Path_mesh.insert(0,"Empty")






Label(Mafenetre, text="Particle parameters", font='Helvetica 18 bold').grid(row=0, column=1, sticky=W)

Seed = IntVar()
Checkbutton(Mafenetre, text="Constant seed", variable=Seed).grid(row=1, column = 1, sticky=W)
Label(Mafenetre, text="Particle number : ").grid(row=2, column=1, sticky=W)
Particle_Number = Entry(Mafenetre)
Particle_Number.config(width=7)
Particle_Number.grid(row=2, column=1, sticky=W, padx=144)
Particle_Number.insert(0,"0")
Label(Mafenetre, text="Lenght scale limiter : ").grid(row=3, column=1, sticky=W)
Lambda_limiter = Entry(Mafenetre)
Lambda_limiter.config(width=7)
Lambda_limiter.grid(row=3, column=1, sticky=W, padx=144)
Lambda_limiter.insert(0,"0")








Label(Mafenetre, text="Time parameters", font='Helvetica 18 bold').grid(row=0, column=2, sticky=W)

Evolving_turbulence = IntVar()
Checkbutton(Mafenetre, text="Evolving turbulence", variable=Evolving_turbulence).grid(row=1, column = 2, sticky=W)
Label(Mafenetre, text="RPM time step : ").grid(row=2, column=2, sticky=W)
RPM_Time_Step = Entry(Mafenetre)
RPM_Time_Step.config(width=5)
RPM_Time_Step.grid(row=2, column=2, sticky=W,padx=137)
RPM_Time_Step.insert(0,"0")
Label(Mafenetre, text="Acoustic time step : ").grid(row=3, column=2, sticky=W)
Acoustic_Time_Step = Entry(Mafenetre)
Acoustic_Time_Step.config(width=5)
Acoustic_Time_Step.grid(row=3, column=2, sticky=W,padx=137)
Acoustic_Time_Step.insert(0,"0")
Label(Mafenetre, text="Final time : ").grid(row=4, column=2, sticky=W)
Final_time = Entry(Mafenetre)
Final_time.config(width=5)
Final_time.grid(row=4, column=2, sticky=W,padx=137)
Final_time.insert(0,"0")






Label(Mafenetre, text="Writing parameters", font='Helvetica 18 bold').grid(row=0, column=3, sticky=W)














Line_second_part = 8


Label(Mafenetre, text="Evaluation parameters", font='Helvetica 18 bold').grid(row=Line_second_part, column=0, sticky=W)

Label(Mafenetre, text="Filter type : ").grid(row=Line_second_part+1, column=0, sticky=W)
Filter_type = StringVar()
Radiobutton(Mafenetre, text="Gaussian", variable=Filter_type, value="Gaussian").grid(row=Line_second_part+2, column = 0, sticky=W)
Radiobutton(Mafenetre, text="Liepmann", variable=Filter_type, value="Liepmann").grid(row=Line_second_part+3, column = 0, sticky=W)
Radiobutton(Mafenetre, text="VonKarman", variable=Filter_type, value="VonKarman").grid(row=Line_second_part+4, column = 0, sticky=W)
Filter_type.set("Gaussian")

Label(Mafenetre, text="Scaling type : ").grid(row=Line_second_part+5, column=0, sticky=W)
Scaling_type = StringVar()
Radiobutton(Mafenetre, text="Particle", variable=Scaling_type, value="particle").grid(row=Line_second_part+6, column = 0, sticky=W)
Radiobutton(Mafenetre, text="Grid-Particle", variable=Scaling_type, value="kGrid_lambdaParticle").grid(row=Line_second_part+7, column = 0, sticky=W)
Radiobutton(Mafenetre, text="Grid", variable=Scaling_type, value="grid").grid(row=Line_second_part+8, column = 0, sticky=W)
Scaling_type.set("particle")

Label(Mafenetre, text="Radius of consideration : ").grid(row=Line_second_part+9, column=0, sticky=W)
Radius_consideration = Entry(Mafenetre)
Radius_consideration.config(width=2)
Radius_consideration.grid(row=Line_second_part+9, column=0, padx = 165)
Radius_consideration.insert(0,"0")




Label(Mafenetre, text="Windowing parameters", font='Helvetica 18 bold').grid(row=Line_second_part, column=1, sticky=W)







Label(Mafenetre, text="Statistics parameters", font='Helvetica 18 bold').grid(row=Line_second_part, column=2, sticky=W)













#Bouton quitter
Button(Mafenetre, text='Close', command=Mafenetre.destroy).grid(row=25, column=0, sticky=W)
Button(Mafenetre, text='Make File', command=get_entry_fields).grid(row=25, column=0, sticky=W, padx=50)


# Lancement du gestionnaire d'événements
Mafenetre.mainloop()
