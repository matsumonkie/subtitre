require 'tempfile'

class SubtitreController < ApplicationController

  def index
    @subtitle = Subtitle.new
  end

  def create
    @subtitle = Subtitle.new(subtitle_params)
    @subtitle.with_temp_file do |tmp|
      dir = File.dirname(tmp.path)
      output = File.basename(tmp.path)

      system(cmd(tmp.path))

      @subtitre = Subtitre.new(file: File.new("#{dir}/#{output}.output"))
    end
  end

  private

  def cmd input_file
    "cd /home/iori/work/subtitre/wall-e && stack exec subtitre-exe #{input_file} en fr Normal"
  end

  def subtitle_params
    params.require(:subtitle).permit(:file)
  end
end
