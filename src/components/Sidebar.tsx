import { useState, useEffect } from 'react';
import { ChevronRight, ChevronDown, BookOpen, Hexagon, PlayCircle } from 'lucide-react';
import { Week, Session } from '../data/courseContent';

interface SidebarProps {
    weeks: Week[];
    currentSession: Session;
    viewMode: 'course' | 'playground';
    onSelectSession: (session: Session) => void;
    onSelectPlayground: () => void;
    isOpen: boolean;
    toggleSidebar?: () => void;
}

const Sidebar = ({ weeks, currentSession, viewMode, onSelectSession, onSelectPlayground, isOpen }: SidebarProps) => {
    const [expandedWeeks, setExpandedWeeks] = useState<Record<number, boolean>>({});

    const toggleWeek = (weekId: number) => {
        setExpandedWeeks(prev => ({ ...prev, [weekId]: !prev[weekId] }));
    };

    // Expand first week by default
    useEffect(() => {
        if (weeks.length > 0 && Object.keys(expandedWeeks).length === 0) {
            setExpandedWeeks({ [weeks[0].id]: true });
        }
    }, [weeks]);

    return (
        <div className={`
      fixed inset-y-0 left-0 z-20 w-80 bg-[var(--sidebar-bg)] border-r border-[var(--border-color)] transform transition-transform duration-300 ease-in-out
      ${isOpen ? 'translate-x-0' : '-translate-x-full'}
      md:relative md:translate-x-0
    `}>
            <div className="p-6 border-b border-white/10 flex items-center gap-3">
                <div className="w-10 h-10 rounded-xl bg-gradient-to-br from-blue-500 to-purple-600 flex items-center justify-center text-white font-bold">
                    <Hexagon size={24} fill="currentColor" className="text-white/20" />
                </div>
                <div>
                    <h1 className="text-lg font-bold text-white tracking-tight">ABAP Master</h1>
                    <p className="text-xs text-slate-400">Portal de Capacitación</p>
                </div>
            </div>

            <div className="overflow-y-auto h-[calc(100vh-88px)] p-4 space-y-4">
                {/* Herramientas / Tools */}
                <div className="space-y-1">
                    <p className="px-3 text-[10px] font-bold text-slate-500 uppercase tracking-widest mb-2">Herramientas</p>
                    <button
                        onClick={onSelectPlayground}
                        className={`
                            w-full flex items-center gap-3 px-4 py-3 rounded-xl text-sm font-semibold transition-all border
                            ${viewMode === 'playground'
                                ? 'bg-blue-600 border-blue-400 text-white shadow-lg shadow-blue-900/40'
                                : 'bg-slate-800/50 border-white/5 text-slate-300 hover:bg-white/5 hover:text-white'}
                        `}
                    >
                        <PlayCircle size={18} className={viewMode === 'playground' ? 'text-white' : 'text-blue-400'} />
                        ABAP Playground
                    </button>
                </div>

                <div className="space-y-2">
                    <p className="px-3 text-[10px] font-bold text-slate-500 uppercase tracking-widest mb-2">Contenido del Curso</p>
                    {weeks.map((week) => (
                        <div key={week.id} className="rounded-lg overflow-hidden bg-slate-800/30 border border-white/5">
                            <button
                                onClick={() => toggleWeek(week.id)}
                                className="w-full flex items-center justify-between p-3 text-left hover:bg-white/5 transition-colors"
                            >
                                <div className="flex items-center gap-3">
                                    <span className="flex items-center justify-center w-6 h-6 rounded bg-slate-700 text-xs font-mono text-slate-300">
                                        S{week.id}
                                    </span>
                                    <span className="text-sm font-medium text-slate-200">
                                        {week.title.split(':')[0]}
                                    </span>
                                </div>
                                {expandedWeeks[week.id] ? <ChevronDown size={14} /> : <ChevronRight size={14} />}
                            </button>

                            {expandedWeeks[week.id] && (
                                <div className="bg-black/20 pb-2">
                                    {week.sessions.map((session) => (
                                        <button
                                            key={session.id}
                                            onClick={() => onSelectSession(session)}
                                            className={`
                      w-full flex items-center gap-3 px-4 py-2 text-sm transition-all border-l-2
                      ${currentSession?.id === session.id
                                                    ? 'border-blue-500 bg-blue-500/10 text-blue-200'
                                                    : 'border-transparent text-slate-400 hover:text-slate-100 hover:bg-white/5'}
                    `}
                                        >
                                            <BookOpen size={14} className={currentSession?.id === session.id ? 'opacity-100' : 'opacity-50'} />
                                            <span className="truncate">{session.title.split(':')[1] || session.title}</span>
                                        </button>
                                    ))}
                                </div>
                            )}
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
};

export default Sidebar;
