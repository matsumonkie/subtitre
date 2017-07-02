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

      system(cmd(tmp.path, @subtitle.mode_text))

      @subtitre = Subtitre.new(file: File.new("#{dir}/#{output}.output"))
    end
  end

  private

  def cmd input_file, level
    "cd /home/iori/work/subtitre/wall-e && stack exec subtitre-exe #{input_file} en fr #{level}"
  end

  def subtitle_params
    params.require(:subtitle).permit(:file, :mode)
  end
end